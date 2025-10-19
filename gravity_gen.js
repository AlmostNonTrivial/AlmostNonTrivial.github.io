// Auto-generated WebGPU binding code
// Generated from shader bindings

function createGridPipelineResources(device) {
    const bindGroupLayout = device.createBindGroupLayout({
        entries: [
            {
                binding: 0,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'uniform' }
            },
            {
                binding: 1,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'read-only-storage' }
            },
            {
                binding: 2,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'uniform' }
            },
        ]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    return { bindGroupLayout, pipelineLayout };
}

function createGridPipeline(device, resources, format, vertexShader, fragShader) {
    return device.createRenderPipeline({
        layout: resources.pipelineLayout,
        vertex: {
            module: device.createShaderModule({ code: vertexShader }),
            entryPoint: 'main',
            buffers: [
                {
                    arrayStride: 8,
                    attributes: [{
                        shaderLocation: 0,
                        offset: 0,
                        format: 'float32x2',
                    }],
                },
            ],
        },
        fragment: {
            module: device.createShaderModule({ code: fragShader }),
            entryPoint: 'main',
            targets: [{
                format: format,
                blend: createBlend()
            }],
        },
        primitive: {
            topology: 'line-list',
            cullMode: 'none',
        },
        depthStencil: createDepthStencil()
    });
}

function createGridBindGroup(device, layout, [uniforms, potential, depthScale]) {
    return device.createBindGroup({
        layout: layout,
        entries: [
            {
                binding: 0,
                resource: { buffer: uniforms }
            },
            {
                binding: 1,
                resource: { buffer: potential }
            },
            {
                binding: 2,
                resource: { buffer: depthScale }
            },
        ]
    });
}

function createSpherePipelineResources(device) {
    const bindGroupLayout = device.createBindGroupLayout({
        entries: [
            {
                binding: 0,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'uniform' }
            },
            {
                binding: 1,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'read-only-storage' }
            },
            {
                binding: 2,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'read-only-storage' }
            },
            {
                binding: 3,
                visibility: GPUShaderStage.VERTEX,
                buffer: { type: 'uniform' }
            },
        ]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    return { bindGroupLayout, pipelineLayout };
}

function createSpherePipeline(device, resources, format) {
    return device.createRenderPipeline({
        layout: resources.pipelineLayout,
        vertex: {
            module: device.createShaderModule({ code: sphereVertexShaderCode }),
            entryPoint: 'main',
            buffers: [
                {
                    arrayStride: 12,
                    attributes: [{
                        shaderLocation: 0,
                        offset: 0,
                        format: 'float32x3',
                    }],
                },
                {
                    arrayStride: 12,
                    attributes: [{
                        shaderLocation: 1,
                        offset: 0,
                        format: 'float32x3',
                    }],
                },
            ],
        },
        fragment: {
            module: device.createShaderModule({ code: sphereFragmentShaderCode }),
            entryPoint: 'main',
            targets: [{
                format: format,
            }],
        },
        primitive: {
            topology: 'triangle-list',
            cullMode: 'none',
        },
        depthStencil: createDepthStencil()
    });
}

function createSphereBindGroup(device, layout, [uniforms, spheres, potential, params]) {
    return device.createBindGroup({
        layout: layout,
        entries: [
            {
                binding: 0,
                resource: { buffer: uniforms }
            },
            {
                binding: 1,
                resource: { buffer: spheres }
            },
            {
                binding: 2,
                resource: { buffer: potential }
            },
            {
                binding: 3,
                resource: { buffer: params }
            },
        ]
    });
}

function createDensitySplatPipelineResources(device) {
    const bindGroupLayout = device.createBindGroupLayout({
        entries: [
            {
                binding: 0,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'uniform' }
            },
            {
                binding: 1,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'read-only-storage' }
            },
            {
                binding: 2,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'uniform' }
            },
            {
                binding: 3,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'storage' }
            },
        ]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    return { bindGroupLayout, pipelineLayout };
}

function createDensitySplatPipeline(device, resources) {
    return device.createComputePipeline({
        layout: resources.pipelineLayout,
        compute: {
            module: device.createShaderModule({ code: densitySplatShaderCode }),
            entryPoint: 'main',
        }
    });
}

function createDensitySplatBindGroup(device, layout, [gridParams, spheres, numSpheres, density]) {
    return device.createBindGroup({
        layout: layout,
        entries: [
            {
                binding: 0,
                resource: { buffer: gridParams }
            },
            {
                binding: 1,
                resource: { buffer: spheres }
            },
            {
                binding: 2,
                resource: { buffer: numSpheres }
            },
            {
                binding: 3,
                resource: { buffer: density }
            },
        ]
    });
}

function createPoissonSolvePipelineResources(device) {
    const bindGroupLayout = device.createBindGroupLayout({
        entries: [
            {
                binding: 0,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'uniform' }
            },
            {
                binding: 1,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'read-only-storage' }
            },
            {
                binding: 2,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'read-only-storage' }
            },
            {
                binding: 3,
                visibility: GPUShaderStage.COMPUTE,
                buffer: { type: 'storage' }
            },
        ]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    return { bindGroupLayout, pipelineLayout };
}

function createPoissonSolvePipeline(device, resources) {
    return device.createComputePipeline({
        layout: resources.pipelineLayout,
        compute: {
            module: device.createShaderModule({ code: poissonSolveShaderCode }),
            entryPoint: 'main',
        }
    });
}

function createPoissonSolveBindGroup(device, layout, [gridParams, density, potentialIn, potentialOut]) {
    return device.createBindGroup({
        layout: layout,
        entries: [
            {
                binding: 0,
                resource: { buffer: gridParams }
            },
            {
                binding: 1,
                resource: { buffer: density }
            },
            {
                binding: 2,
                resource: { buffer: potentialIn }
            },
            {
                binding: 3,
                resource: { buffer: potentialOut }
            },
        ]
    });
}
