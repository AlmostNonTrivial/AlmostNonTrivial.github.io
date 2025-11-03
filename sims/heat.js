/*
Heat Equation with Implicit Time-Stepping:

The heat equation describes how temperature spreads from hot regions to cold regions over time. Instead of using explicit time-stepping we use an implicit method where each new temperature value depends on its neighbors at the next timestep. This creates a system of equations that we solve iteratively using the Jacobi method, repeatedly averaging each cell's temperature with its neighbors until the solution converges (nothing meaningful changes per iteration). Random heat pulses are periodically added to the grid. The visualization maps temperature to a color gradient from dark blue (cold) through cyan and yellow to red (hot), with height also scaled by temperature to create a 3D surface."
*/

const GRID_SIZE = 120;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;
const HEAT_SPAWN_INTERVAL = 520;
const ALPHA = 0.001;
const DT = 0.2;
const JACOBI_ITERATIONS = 20;

const height_color_frag = `
@fragment
fn main(@location(0) height: f32, @location(1) distance: f32) -> @location(0) vec4<f32> {
    let T = clamp(height / 100.0, 0.0, 1.0);

    var color: vec3<f32>;
    if (T < 0.25) {
        let t = T * 4.0;
        color = mix(vec3<f32>(0.0, 0.0, 0.3), vec3<f32>(0.0, 0.5, 1.0), t);
    } else if (T < 0.5) {
        let t = (T - 0.25) * 4.0;
        color = mix(vec3<f32>(0.0, 0.5, 1.0), vec3<f32>(0.0, 1.0, 1.0), t);
    } else if (T < 0.75) {
        let t = (T - 0.5) * 4.0;
        color = mix(vec3<f32>(0.0, 1.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), t);
    } else {
        let t = (T - 0.75) * 4.0;
        color = mix(vec3<f32>(1.0, 1.0, 0.0), vec3<f32>(1.0, 0.0, 0.0), t);
    }

    let fadeStart = 8.0;
    let fadeEnd = 14.0;
    let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);
    return vec4<f32>(color, alpha);
}
`;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> temperature: array<f32>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) temp: f32,
    @location(1) distance: f32,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let T = temperature[input.vertexIndex];
    let heightScale = 0.02;
    let y = T * heightScale;
    let pos = vec3<f32>(input.xz.x, y, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.temp = T;
    output.distance = length(pos);
    return output;
}
`;

const computeShaderCode = `
@binding(0) @group(0) var<storage, read> tempIn: array<f32>;
@binding(1) @group(0) var<storage, read_write> tempOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getTemp(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return 0.0;
    }
    return tempIn[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    if (x == 0 || x == ${GRID_SIZE - 1} || y == 0 || y == ${GRID_SIZE - 1}) {
        tempOut[i] = 0.0;
        return;
    }

    let T_center = tempIn[i];
    let T_left = getTemp(x - 1, y);
    let T_right = getTemp(x + 1, y);
    let T_bottom = getTemp(x, y - 1);
    let T_top = getTemp(x, y + 1);

    let dx = ${GRID_SPACING};
    let sum_neighbors = T_left + T_right + T_top + T_bottom;
    let a = ${DT} * ${ALPHA} / (dx * dx);

    tempOut[i] = (T_center + a * sum_neighbors) / (1.0 + 4.0 * a);
}
`;

const addHeatSourceShaderCode = `
@binding(0) @group(0) var<uniform> sourceParams: vec4f;
@binding(1) @group(0) var<storage, read> tempIn: array<f32>;
@binding(2) @group(0) var<storage, read_write> tempOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let i = cellIndex(cell.x, cell.y);
    let centerX = sourceParams.x;
    let centerY = sourceParams.y;
    let magnitude = sourceParams.z;
    let radius = sourceParams.w;

    let dx = f32(cell.x) - centerX;
    let dy = f32(cell.y) - centerY;
    let dist2 = dx * dx + dy * dy;
    let heat = magnitude * exp(-dist2 / (radius * radius));

    tempOut[i] = tempIn[i] + heat;
}
`;

async function initHeatImplicitJacobi() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);

  const { xzBuffer, indices, indexBuffer } = gridBuffers(
    device,
    GRID_SIZE,
    GRID_SPACING,
  );
  let depthTexture = createDepthTexture(device, canvas);

  const tempBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  device.queue.writeBuffer(tempBuffers[0], 0, new Float32Array(CELL_COUNT));

  const mvpBuffer = createUniformBuffer(device, 64);
  const heatSourceParamsBuffer = createUniformBuffer(device, 16);

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    height_color_frag,
    format,
  );

  const computePipeline = create_compute_pipeline(device, computeShaderCode);

  const addHeatSourcePipeline = create_compute_pipeline(
    device,
    addHeatSourceShaderCode,
  );

  let step = 0;
  let frameCount = 0;

  function addHeatPulse() {
    const randomX = Math.random() * (GRID_SIZE / 2);
    const randomY = Math.random() * (GRID_SIZE / 2);
    const radius = 10;
    const magnitude = 100.0;

    writeBuffer(
      device,
      heatSourceParamsBuffer,
      new Float32Array([randomX, randomY, magnitude, radius]),
    );

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        addHeatSourcePipeline,
        [
          heatSourceParamsBuffer,
          tempBuffers[step % 2],
          tempBuffers[(step + 1) % 2],
        ],
        WORKGROUP_COUNT,
      );
    });
    step++;
  }

  const camera = createOrbitCamera({
    radius: 15.0,
    height: 5.0,
    speed: 0.0002,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.05, g: 0.05, b: 0.1, a: 1.0 },
  });

  addHeatPulse();

  function render() {


    if (frameCount % HEAT_SPAWN_INTERVAL === 0) {
      addHeatPulse();
    }


    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

    frameCount++;

    for (let iter = 0; iter < JACOBI_ITERATIONS; iter++) {
      submit_commands(device, (encoder) => {
        dispatch(
          device,
          encoder,
          computePipeline,
          [tempBuffers[step % 2], tempBuffers[(step + 1) % 2]],
          WORKGROUP_COUNT,
        );
      });
      step++;
    }

    submit_commands(device, (encoder) => {
      const renderPass = beginColorRenderPass(
        encoder,
        context,
        renderPassConfig,
        depthTexture,
      );

      draw(
        device,
        renderPass,
        renderPipeline,
        [mvpBuffer, tempBuffers[step % 2]],
        xzBuffer,
        indexBuffer,
        indices.length,
      );

      renderPass.end();
    });

    requestAnimationFrame(render);
  }

  render();
}

initHeatImplicitJacobi();