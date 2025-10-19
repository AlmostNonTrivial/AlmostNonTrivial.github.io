const GRID_SIZE = 150;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const POISSON_ITERATIONS = 80;
const DEPTH_SCALE = 0.15;
const FOUR_PI_G = 8.0;
const NUM_SPHERES = 1;

const gridVertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> potential: array<f32>;
@binding(2) @group(0) var<uniform> depthScale: f32;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) depth: f32,
    @location(1) distance: f32,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let phi = potential[input.vertexIndex];
    let displacement = phi * depthScale;
    
    let pos = vec3<f32>(input.xz.x, displacement, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.depth = -displacement;
    output.distance = length(pos);
    return output;
}
`;

const gridFragmentShaderCode = `
@fragment
fn main(@location(0) depth: f32, @location(1) distance: f32) -> @location(0) vec4<f32> {
    let fadeStart = 8.0;
    let fadeEnd = 14.0;
    let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);
    return vec4<f32>(0.2, 0.2, 0.2, alpha * 0.9);
}
`;

const sphereVertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> spheres: array<vec4f>;
@binding(2) @group(0) var<storage, read> potential: array<f32>;
@binding(3) @group(0) var<uniform> params: vec4f;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) normal: vec3<f32>,
    @location(1) worldPos: vec3<f32>,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    
    let sphereIdx = u32(params.w);
    let sphere = spheres[sphereIdx];
    let spherePos = sphere.xy;
    let sphereRadius = sphere.z;
    
    let worldPos = vec3<f32>(spherePos.x, 0.0, spherePos.y) + input.position * sphereRadius;
    
    output.position = uniforms.mvp * vec4<f32>(worldPos, 1.0);
    output.normal = input.normal;
    output.worldPos = worldPos;
    return output;
}
`;

const sphereFragmentShaderCode = `
@fragment
fn main(@location(0) normal: vec3<f32>, @location(1) worldPos: vec3<f32>) -> @location(0) vec4<f32> {
    let lightDir = normalize(vec3<f32>(0.5, 1.0, 0.3));
    let diffuse = max(dot(normalize(normal), lightDir), 0.0);
    let ambient = 0.3;
    let lighting = ambient + diffuse * 0.7;
    
    let baseColor = vec3<f32>(0.2, 0.2, 0.2);
    return vec4<f32>(baseColor * lighting, 1.0);
}
`;

const densitySplatShaderCode = `
@binding(0) @group(0) var<uniform> gridParams: vec4f;
@binding(1) @group(0) var<storage, read> spheres: array<vec4f>;
@binding(2) @group(0) var<uniform> numSpheres: u32;
@binding(3) @group(0) var<storage, read_write> density: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * u32(gridParams.x) + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    let gridSize = u32(gridParams.x);
    if (cell.x >= gridSize || cell.y >= gridSize) {
        return;
    }
    
    let gridSpacing = gridParams.y;
    let offset = (f32(gridSize) - 1.0) * gridSpacing * 0.5;
    
    let cellWorldPos = vec2f(
        f32(cell.x) * gridSpacing - offset,
        f32(cell.y) * gridSpacing - offset
    );
    
    var totalDensity = 0.0;
    
    for (var i = 0u; i < numSpheres; i++) {
        let sphere = spheres[i];
        let spherePos = sphere.xy;
        let mass = sphere.z;
        let radius = sphere.w;
        
        let dist = length(cellWorldPos - spherePos);
        let falloff = mass * exp(-dist * dist / (radius * radius));
        totalDensity += falloff;
    }
    
    density[cellIndex(cell.x, cell.y)] = totalDensity;
}
`;

const poissonSolveShaderCode = `
@binding(0) @group(0) var<uniform> gridParams: vec4f;
@binding(1) @group(0) var<storage, read> density: array<f32>;
@binding(2) @group(0) var<storage, read> potentialIn: array<f32>;
@binding(3) @group(0) var<storage, read_write> potentialOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * u32(gridParams.x) + x;
}

fn getPotential(x: i32, y: i32) -> f32 {
    let gridSize = i32(gridParams.x);
    if (x < 0 || x >= gridSize || y < 0 || y >= gridSize) {
        return 0.0;
    }
    return potentialIn[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    let gridSize = u32(gridParams.x);
    if (cell.x >= gridSize || cell.y >= gridSize) {
        return;
    }
    
    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);
    
    let boundaryWidth = 2;
    if (x < boundaryWidth || x >= i32(gridSize) - boundaryWidth ||
        y < boundaryWidth || y >= i32(gridSize) - boundaryWidth) {
        potentialOut[i] = 0.0;
        return;
    }
    
    let left = getPotential(x - 1, y);
    let right = getPotential(x + 1, y);
    let bottom = getPotential(x, y - 1);
    let top = getPotential(x, y + 1);
    let source = density[i];
    
    let h = gridParams.y;
    let fourPiG = gridParams.z;
    
    potentialOut[i] = (left + right + bottom + top - h * h * fourPiG * source) * 0.25;
}
`;

async function initGravitySim() {
    const canvas = document.getElementById('canvas');

    try {
        const { device, context, format } = await initWebGPU(canvas);

        const xzPositions = generateGridXZ(GRID_SIZE, GRID_SPACING);
        const xzBuffer = createVertexBuffer(device, xzPositions);

        const indices = generateGridIndices(GRID_SIZE);
        const indexBuffer = createIndexBuffer(device, indices);

        const sphere = generateSphere(20);
        const sphereVertexBuffer = createVertexBuffer(device, sphere.positions);
        const sphereNormalBuffer = createVertexBuffer(device, sphere.normals);
        const sphereIndexBuffer = createIndexBuffer(device, sphere.indices);

        const cellCount = GRID_SIZE * GRID_SIZE;

        const potentialBuffers = [
            createStorageBuffer(device, cellCount * 4),
            createStorageBuffer(device, cellCount * 4)
        ];

        const densityBuffer = createStorageBuffer(device, cellCount * 4);
        const sphereDataBuffer = createStorageBuffer(device, NUM_SPHERES * 16);
        const mvpBuffer = createUniformBuffer(device, 64);

        const gridParamsBuffer = createUniformBuffer(device, 16);
        device.queue.writeBuffer(gridParamsBuffer, 0, new Float32Array([GRID_SIZE, GRID_SPACING, FOUR_PI_G, 0]));

        const numSpheresBuffer = createUniformBuffer(device, 4);
        device.queue.writeBuffer(numSpheresBuffer, 0, new Uint32Array([NUM_SPHERES]));

        const depthScaleBuffer = createUniformBuffer(device, 4);
        device.queue.writeBuffer(depthScaleBuffer, 0, new Float32Array([DEPTH_SCALE]));

        const sphereParamsBuffer = createUniformBuffer(device, 16);

        const gridRenderResources = createGridPipelineResources(device);
        const sphereRenderResources = createSpherePipelineResources(device);
        const densitySplatResources = createDensitySplatPipelineResources(device);
        const poissonSolveResources = createPoissonSolvePipelineResources(device);

        const gridRenderPipeline = createGridPipeline(device, gridRenderResources, format, gridVertexShaderCode, gridFragmentShaderCode);

        const sphereRenderPipeline = createSpherePipeline(device, sphereRenderResources, format);

        const densitySplatPipeline = createDensitySplatPipeline(device, densitySplatResources);

        const poissonSolvePipeline = createPoissonSolvePipeline(device, poissonSolveResources);

        let depthTexture = null;
        let step = 0;

        function updateSpheres(time) {
            const sphereData = new Float32Array(NUM_SPHERES * 4);

            const orbit1Radius = 3.0;
            const orbit1Speed = 0.3;
            sphereData[0] = Math.cos(time * orbit1Speed) * orbit1Radius;
            sphereData[1] = Math.sin(time * orbit1Speed) * orbit1Radius;
            sphereData[2] = 1.0;
            sphereData[3] = 1.5;

            device.queue.writeBuffer(sphereDataBuffer, 0, sphereData);
        }

        function simulateStep() {
            const workgroupCount = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);

            const encoder = device.createCommandEncoder();

            const splatBindGroup = createDensitySplatBindGroup(device, densitySplatResources.bindGroupLayout, [
                gridParamsBuffer,
                sphereDataBuffer,
                numSpheresBuffer,
                densityBuffer
            ]);

            dispatchCompute(encoder, densitySplatPipeline, splatBindGroup, workgroupCount, workgroupCount);

            device.queue.submit([encoder.finish()]);

            for (let iter = 0; iter < POISSON_ITERATIONS; iter++) {
                const encoder2 = device.createCommandEncoder();
                const poissonBindGroup = createPoissonSolveBindGroup(device, poissonSolveResources.bindGroupLayout, [
                    gridParamsBuffer,
                    densityBuffer,
                    potentialBuffers[step % 2],
                    potentialBuffers[(step + 1) % 2]
                ]);

                dispatchCompute(encoder2, poissonSolvePipeline, poissonBindGroup, workgroupCount, workgroupCount);

                device.queue.submit([encoder2.finish()]);
                step++;
            }
        }


        const camera = createOrbitCamera({
            radius: 11.0,
            height: 2.0,
            speed: 0.0002
        });

        const renderPassConfig = createRenderPassConfig({
            clearColor: { r: 0.1, g: 0.1, b: 0.1, a: 1.0 }
        });



        function render() {
            const width = window.innerWidth;
            const height = window.innerHeight;

            if (canvas.width !== width || canvas.height !== height) {
                canvas.width = width;
                canvas.height = height;

                if (depthTexture) {
                    depthTexture.destroy();
                }
                depthTexture = device.createTexture({
                    size: [width, height],
                    format: 'depth24plus',
                    usage: GPUTextureUsage.RENDER_ATTACHMENT,
                });
            }

            const time = Date.now() * 0.001;
            updateSpheres(time);
            simulateStep();

            const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
            device.queue.writeBuffer(mvpBuffer, 0, mvp);

            const encoder = device.createCommandEncoder();

            const renderPass = encoder.beginRenderPass({
                colorAttachments: [{
                    view: context.getCurrentTexture().createView(),
                    clearValue: { r: 0.02, g: 0.02, b: 0.05, a: 1.0 },
                    loadOp: 'clear',
                    storeOp: 'store',
                }],
                depthStencilAttachment: {
                    view: depthTexture.createView(),
                    depthClearValue: 1.0,
                    depthLoadOp: 'clear',
                    depthStoreOp: 'store',
                },
            });


            const gridBindGroup = createGridBindGroup(device, gridRenderResources.bindGroupLayout, [
                mvpBuffer,
                potentialBuffers[step % 2],
                depthScaleBuffer
            ]);

            drawIndexed(renderPass, gridRenderPipeline, gridBindGroup, xzBuffer, indexBuffer, indices.length);

            for (let i = 0; i < NUM_SPHERES; i++) {
                device.queue.writeBuffer(sphereParamsBuffer, 0, new Float32Array([GRID_SIZE, GRID_SPACING, DEPTH_SCALE, i]));
                const sphereBindGroup = createSphereBindGroup(device, sphereRenderResources.bindGroupLayout, [
                    mvpBuffer,
                    sphereDataBuffer,
                    potentialBuffers[step % 2],
                    sphereParamsBuffer
                ]);

                drawIndexed(renderPass, sphereRenderPipeline, sphereBindGroup, [sphereVertexBuffer, sphereNormalBuffer], sphereIndexBuffer, sphere.indices.length)
            }

            renderPass.end();

            device.queue.submit([encoder.finish()]);
            requestAnimationFrame(render);
        }

        render();
    } catch (error) {
        console.error(error.message);
    }
}

initGravitySim();