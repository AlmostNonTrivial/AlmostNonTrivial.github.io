function createUniformBuffer(device, data) {

    if (typeof data === 'number') {
        return device.createBuffer({
            size: data,
            usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
        });
    }

    const buffer = device.createBuffer({
        size: data.byteLength,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(buffer, 0, data);
    return buffer;
}

function createStorageBuffer(device, size) {
    return device.createBuffer({
        size: size,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
    });
}

function createVertexBuffer(device, data) {
    const buffer = device.createBuffer({
        size: data.byteLength,
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(buffer, 0, data);
    return buffer;
}

function createIndexBuffer(device, data) {
    const buffer = device.createBuffer({
        size: data.byteLength,
        usage: GPUBufferUsage.INDEX | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(buffer, 0, data);
    return buffer;
}

async function initWebGPU(canvas) {
    if (!navigator.gpu) throw new Error('WebGPU not supported');
    const adapter = await navigator.gpu.requestAdapter();
    if (!adapter) throw new Error('No adapter found');
    const device = await adapter.requestDevice();
    const context = canvas.getContext('webgpu');
    const format = navigator.gpu.getPreferredCanvasFormat();
    context.configure({
        device: device,
        format: format,
        alphaMode: 'opaque',
    });
    return { device, context, format };
}

function createPerspective(fov, aspect, near, far) {
    const f = 1.0 / Math.tan(fov / 2);
    const nf = 1 / (near - far);
    return new Float32Array([
        f / aspect, 0, 0, 0,
        0, f, 0, 0,
        0, 0, far * nf, -1,
        0, 0, far * near * nf, 0
    ]);
}

function createLookAt(eye, target, up) {
    const zAxis = normalize(subtract(eye, target));
    const xAxis = normalize(cross(up, zAxis));
    const yAxis = cross(zAxis, xAxis);
    return new Float32Array([
        xAxis[0], yAxis[0], zAxis[0], 0,
        xAxis[1], yAxis[1], zAxis[1], 0,
        xAxis[2], yAxis[2], zAxis[2], 0,
        -dot(xAxis, eye), -dot(yAxis, eye), -dot(zAxis, eye), 1
    ]);
}

function multiply(a, b) {
    const result = new Float32Array(16);
    for (let row = 0; row < 4; row++) {
        for (let col = 0; col < 4; col++) {
            result[col * 4 + row] =
                a[0 * 4 + row] * b[col * 4 + 0] +
                a[1 * 4 + row] * b[col * 4 + 1] +
                a[2 * 4 + row] * b[col * 4 + 2] +
                a[3 * 4 + row] * b[col * 4 + 3];
        }
    }
    return result;
}

function subtract(a, b) {
    return [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
}

function cross(a, b) {
    return [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0]
    ];
}

function dot(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

function normalize(v) {
    const len = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
    return [v[0] / len, v[1] / len, v[2] / len];
}

function generateGridXZ(size, spacing) {
    const positions = [];
    const offset = (size - 1) * spacing * 0.5;

    for (let i = 0; i < size; i++) {
        for (let j = 0; j < size; j++) {
            positions.push(
                j * spacing - offset,
                i * spacing - offset
            );
        }
    }
    return new Float32Array(positions);
}

function generateGridIndices(size) {
    const indices = [];

    for (let i = 0; i < size; i++) {
        for (let j = 0; j < size - 1; j++) {
            const idx = i * size + j;
            indices.push(idx, idx + 1);
        }
    }

    for (let i = 0; i < size - 1; i++) {
        for (let j = 0; j < size; j++) {
            const idx = i * size + j;
            indices.push(idx, idx + size);
        }
    }

    return new Uint32Array(indices);
}

function generateSphere(subdivisions) {
    const positions = [];
    const normals = [];
    const indices = [];

    for (let lat = 0; lat <= subdivisions; lat++) {
        const theta = lat * Math.PI / subdivisions;
        const sinTheta = Math.sin(theta);
        const cosTheta = Math.cos(theta);

        for (let lon = 0; lon <= subdivisions; lon++) {
            const phi = lon * 2 * Math.PI / subdivisions;
            const sinPhi = Math.sin(phi);
            const cosPhi = Math.cos(phi);

            const x = cosPhi * sinTheta;
            const y = cosTheta;
            const z = sinPhi * sinTheta;

            positions.push(x, y, z);
            normals.push(x, y, z);
        }
    }

    for (let lat = 0; lat < subdivisions; lat++) {
        for (let lon = 0; lon < subdivisions; lon++) {
            const first = lat * (subdivisions + 1) + lon;
            const second = first + subdivisions + 1;

            indices.push(first, second, first + 1);
            indices.push(second, second + 1, first + 1);
        }
    }

    return {
        positions: new Float32Array(positions),
        normals: new Float32Array(normals),
        indices: new Uint32Array(indices)
    };
}

function createOrbitCamera(config) {
    return {
        radius: config.radius || 7.0,
        height: config.height || 4.0,
        speed: config.speed || 1.0,
        fov: config.fov || Math.PI / 4,
        near: config.near || 0.1,
        far: config.far || 100.0,
        target: config.target || [0, 0, 0],
        up: config.up || [0, 1, 0]
    };
}

function updateOrbitCamera(camera, time, aspect) {
    const angle = time * camera.speed;
    const eye = [
        Math.cos(angle) * camera.radius,
        camera.height,
        Math.sin(angle) * camera.radius
    ];

    const projection = createPerspective(camera.fov, aspect, camera.near, camera.far);
    const view = createLookAt(eye, camera.target, camera.up);
    return multiply(projection, view);
}

function createRenderPassConfig(config = {}) {
    return {
        clearColor: config.clearColor || { r: 0.1, g: 0.1, b: 0.1, a: 1.0 },
        loadOp: config.loadOp || 'clear',
        storeOp: config.storeOp || 'store',
        depthStencilConfig: config.depthStencilConfig || null
    };
}

function beginColorRenderPass(encoder, context, config) {
    const passDescriptor = {
        colorAttachments: [{
            view: context.getCurrentTexture().createView(),
            clearValue: config.clearColor,
            loadOp: config.loadOp,
            storeOp: config.storeOp,
        }]
    };

    if (config.depthStencilConfig) {
        passDescriptor.depthStencilAttachment = {
            view: config.depthStencilConfig.view,
            depthClearValue: config.depthStencilConfig.clearValue || 1.0,
            depthLoadOp: config.depthStencilConfig.loadOp || 'clear',
            depthStoreOp: config.depthStencilConfig.storeOp || 'store',
        };
    }
    if (!encoder) {
        return null;
    }
    return encoder.beginRenderPass(passDescriptor);

}

function handleCanvasResize(canvas) {
    const width = window.innerWidth;
    const height = window.innerHeight;

    if (canvas.width !== width || canvas.height !== height) {
        canvas.width = width;
        canvas.height = height;
        return true;
    }
    return false;
}

function getAspectRatio(canvas) {
    return canvas.width / canvas.height;
}

function dispatchCompute(encoder, pipeline, bindGroup, workgroupX, workgroupY = 1, workgroupZ = 1) {
    const pass = encoder.beginComputePass();
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, bindGroup);
    pass.dispatchWorkgroups(workgroupX, workgroupY, workgroupZ);
    pass.end();
}

function drawIndexed(renderPass, pipeline, bindGroup, vertexBuffer, indexBuffer, indexCount) {
    renderPass.setPipeline(pipeline);
    renderPass.setBindGroup(0, bindGroup);
    if (Array.isArray(vertexBuffer)) {
        for (let i = 0; i < vertexBuffer.length; i++) {
            renderPass.setVertexBuffer(i, vertexBuffer[i]);
        }
    } else {
        renderPass.setVertexBuffer(0, vertexBuffer);
    }

    renderPass.setIndexBuffer(indexBuffer, 'uint32');
    renderPass.drawIndexed(indexCount);
}

function createBlend() {

    return {
        color: {
            srcFactor: 'src-alpha',
            dstFactor: 'one-minus-src-alpha',
            operation: 'add',
        },
        alpha: {
            srcFactor: 'one',
            dstFactor: 'one-minus-src-alpha',
            operation: 'add',
        },
    }
}
function createDepthStencil() {
    return {
        depthWriteEnabled: true,
        depthCompare: 'less',
        format: 'depth24plus',
    }
}