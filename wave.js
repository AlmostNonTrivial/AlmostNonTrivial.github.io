const GRID_SIZE = 160;
const GRID_SPACING = 0.1;
const WORKGROUP_SIZE = 8;
const WAVE_SPAWN_INTERVAL = 180;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> waveState: array<vec2f>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) distance: f32,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let y = waveState[input.vertexIndex].y;
    let pos = vec3<f32>(input.xz.x, y, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.distance = length(pos);
    return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) distance: f32) -> @location(0) vec4<f32> {
    let fadeStart = 5.0;
    let fadeEnd = 8.0;
    let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);
    return vec4<f32>(0.3, 0.3, 0.3, alpha);
}
`;

const computeShaderCode = `
@binding(0) @group(0) var<uniform> grid: vec2f;
@binding(1) @group(0) var<storage> stateIn: array<vec2f>;
@binding(2) @group(0) var<storage, read_write> stateOut: array<vec2f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * u32(grid.x) + x;
}

fn cellHeightSafe(x: i32, y: i32) -> f32 {
    let gx = i32(grid.x);
    let gy = i32(grid.y);

    if (x < 0 || x >= gx || y < 0 || y >= gy) {
        return 0.0;
    }
    return stateIn[cellIndex(u32(x), u32(y))].y;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= u32(grid.x) || cell.y >= u32(grid.y)) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let boundaryWidth = 2;
    let atBoundary = x < boundaryWidth || x >= i32(grid.x) - boundaryWidth || 
                     y < boundaryWidth || y >= i32(grid.y) - boundaryWidth;

    if (atBoundary) {
        stateOut[i].x = stateIn[i].x * 0.9;
        stateOut[i].y = stateIn[i].y * 0.9;
        return;
    }

    let h_c = cellHeightSafe(x, y);
    let h_n = cellHeightSafe(x, y+1);
    let h_s = cellHeightSafe(x, y-1);
    let h_e = cellHeightSafe(x+1, y);
    let h_w = cellHeightSafe(x-1, y);
    let h_ne = cellHeightSafe(x+1, y+1);
    let h_nw = cellHeightSafe(x-1, y+1);
    let h_se = cellHeightSafe(x+1, y-1);
    let h_sw = cellHeightSafe(x-1, y-1);

    let laplacian = (4.0 * (h_n + h_s + h_e + h_w) + 
                    (h_ne + h_nw + h_se + h_sw) - 
                    20.0 * h_c) / 6.0;

    let dt = 0.09;
    let c = 1.0;
    let cs = c * c;
    let a = cs * laplacian;

    let v = stateIn[i].x;
    let h = stateIn[i].y;

    let v_new = (v + a * dt) * 0.995;
    let h_new = h + v_new * dt;

    stateOut[i].x = v_new;
    stateOut[i].y = h_new;
}
`;

async function initBackdrop() {
    const canvas = document.getElementById('canvas');
    const info = document.getElementById('info');

    try {
        const { device, context, format } = await initWebGPU(canvas);

        const xzPositions = generateGridXZ(GRID_SIZE, GRID_SPACING);
        const xzBuffer = createVertexBuffer(device, xzPositions);

        const indices = generateGridIndices(GRID_SIZE);

        const indexBuffer = createIndexBuffer(device, indices);

        const waveStateArray = new Float32Array(GRID_SIZE * GRID_SIZE * 2);
        for (let i = 0; i < GRID_SIZE * GRID_SIZE; i++) {
            waveStateArray[i * 2] = 0;
            waveStateArray[i * 2 + 1] = 0;
        }
        const centerIdx = Math.floor(GRID_SIZE / 2) * GRID_SIZE + Math.floor(GRID_SIZE / 2);
        waveStateArray[centerIdx * 2 + 1] = 1.0;

        const waveBuffers = [
            createStorageBuffer(device, waveStateArray.byteLength),
            createStorageBuffer(device, waveStateArray.byteLength),
        ];

        device.queue.writeBuffer(waveBuffers[0], 0, waveStateArray);

        const gridUniform = new Float32Array([GRID_SIZE, GRID_SIZE]);
        const gridBuffer = createUniformBuffer(device, gridUniform);

        const mvpBuffer = createUniformBuffer(device, 64);

        const renderResources = createRenderPipelineResources(device);

        const computeResources = createComputePipelineResources(device);

        const renderPipeline = createRenderPipeline(device, renderResources, format);

        const computePipeline = createComputePipeline(device, computeResources);

        const renderBindGroups = [
            createRenderBindGroup(device, renderResources.bindGroupLayout, [mvpBuffer, waveBuffers[0]]),
            createRenderBindGroup(device, renderResources.bindGroupLayout, [mvpBuffer, waveBuffers[1]])
        ];

        const computeBindGroups = [
            createComputeBindGroup(device, computeResources.bindGroupLayout, [gridBuffer, waveBuffers[0], waveBuffers[1]]),
            createComputeBindGroup(device, computeResources.bindGroupLayout, [gridBuffer, waveBuffers[1], waveBuffers[0]])
        ];

        let step = 0;
        let frameCount = 0;

        function addWaveDisturbance() {
            const randomX = Math.floor(Math.random() * GRID_SIZE);
            const randomY = Math.floor(Math.random() * GRID_SIZE);
            const idx = randomY * GRID_SIZE + randomX;

            const disturbance = new Float32Array(2);
            disturbance[0] = 0;
            disturbance[1] = 3;

            const sourceBuffer = waveBuffers[step % 2];
            device.queue.writeBuffer(sourceBuffer, idx * 8, disturbance);
        }

        const camera = createOrbitCamera({
            radius: 7.0,
            height: 4.0,
            speed: 0.0002
        });

        const renderPassConfig = createRenderPassConfig({
            clearColor: { r: 0.1, g: 0.1, b: 0.1, a: 1.0 }
        });



        function render() {

            handleCanvasResize(canvas);
            const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
            device.queue.writeBuffer(mvpBuffer, 0, mvp);


            if (frameCount % WAVE_SPAWN_INTERVAL === 0) {
                addWaveDisturbance();
            }
            frameCount++;

            const encoder = device.createCommandEncoder();

            const workgroup = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
            dispatchCompute(encoder, computePipeline, computeBindGroups[step % 2], workgroup, workgroup);

            step++;


            const renderPass = beginColorRenderPass(
                encoder,
                context,
                renderPassConfig
            );


            drawIndexed(renderPass, renderPipeline, renderBindGroups[step % 2], xzBuffer, indexBuffer, indices.length);
            renderPass.end();

            device.queue.submit([encoder.finish()]);
            requestAnimationFrame(render);
        }

        render();
    } catch (error) {
        info.textContent = `Error: ${error.message}`;
    }
}


initBackdrop();