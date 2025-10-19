// Auto-generated WebGPU binding code
// Generated from shader bindings

function createRenderPipelineResources(device) {
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
        ]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    return { bindGroupLayout, pipelineLayout };
}

function createRenderPipeline(device, resources, format) {
    return device.createRenderPipeline({
        layout: resources.pipelineLayout,
        vertex: {
            module: device.createShaderModule({ code: vertexShaderCode }),
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
            module: device.createShaderModule({ code: fragmentShaderCode }),
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
    });
}

function createRenderBindGroup(device, layout, [uniforms, waveState]) {
    return device.createBindGroup({
        layout: layout,
        entries: [
            {
                binding: 0,
                resource: { buffer: uniforms }
            },
            {
                binding: 1,
                resource: { buffer: waveState }
            },
        ]
    });
}

function createComputePipelineResources(device) {
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
                buffer: { type: 'storage' }
            },
        ]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    return { bindGroupLayout, pipelineLayout };
}

function createComputePipeline(device, resources) {
    return device.createComputePipeline({
        layout: resources.pipelineLayout,
        compute: {
            module: device.createShaderModule({ code: computeShaderCode }),
            entryPoint: 'main',
        }
    });
}

function createComputeBindGroup(device, layout, [grid, stateIn, stateOut]) {
    return device.createBindGroup({
        layout: layout,
        entries: [
            {
                binding: 0,
                resource: { buffer: grid }
            },
            {
                binding: 1,
                resource: { buffer: stateIn }
            },
            {
                binding: 2,
                resource: { buffer: stateOut }
            },
        ]
    });
}
