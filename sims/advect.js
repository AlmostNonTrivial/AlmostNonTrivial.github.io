const GRID_SIZE = 120;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;

const ADVECTION_SPAWN_INTERVAL = 150;
const DT = 0.8;
const DECAY = 0.999;
const ANGULAR_VELOCITY = 1.0;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> scalar: array<f32>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) u: f32,
    @location(1) distance: f32,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let u = scalar[input.vertexIndex];
    let heightScale = 0.02;
    let y = u * heightScale;
    let pos = vec3<f32>(input.xz.x, y, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.u = u;
    output.distance = length(pos);
    return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) u: f32, @location(1) distance: f32) -> @location(0) vec4<f32> {
    let t = clamp(u, 0.0, 1.0);

    var color: vec3<f32>;
    if (t < 0.5) {
        let s = t * 2.0;
        color = mix(vec3<f32>(0.0, 0.0, 0.0), vec3<f32>(0.0, 1.0, 0.0), s);
    } else {
        let s = (t - 0.5) * 2.0;
        color = mix(vec3<f32>(0.0, 1.0, 0.0), vec3<f32>(1.0, 1.0, 1.0), s);
    }

    let fadeStart = 8.0;
    let fadeEnd = 14.0;
    let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);
    return vec4<f32>(color, alpha);
}
`;

const advectionComputeShaderCode = `
@binding(0) @group(0) var<storage, read> scalarIn: array<f32>;
@binding(1) @group(0) var<storage, read_write> scalarOut: array<f32>;
@binding(2) @group(0) var<storage, read> velocity: array<vec2f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn wrapIndex(i: i32) -> u32 {
    let grid = i32(${GRID_SIZE});
    return u32((i + grid) % grid);
}

fn sampleBilinear(pos: vec2f) -> f32 {
    let gridSize = f32(${GRID_SIZE});

    var x = pos.x;
    var y = pos.y;

    while (x < 0.0) { x += gridSize; }
    while (x >= gridSize) { x -= gridSize; }
    while (y < 0.0) { y += gridSize; }
    while (y >= gridSize) { y -= gridSize; }

    let x0 = i32(floor(x));
    let y0 = i32(floor(y));
    let x1 = x0 + 1;
    let y1 = y0 + 1;

    let fx = fract(x);
    let fy = fract(y);

    let v00 = scalarIn[cellIndex(wrapIndex(x0), wrapIndex(y0))];
    let v10 = scalarIn[cellIndex(wrapIndex(x1), wrapIndex(y0))];
    let v01 = scalarIn[cellIndex(wrapIndex(x0), wrapIndex(y1))];
    let v11 = scalarIn[cellIndex(wrapIndex(x1), wrapIndex(y1))];

    let v0 = mix(v00, v10, fx);
    let v1 = mix(v01, v11, fx);

    return mix(v0, v1, fy);
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let i = cellIndex(cell.x, cell.y);
    let vel = velocity[i];

    let currentPos = vec2f(f32(cell.x), f32(cell.y));
    let prevPos = currentPos - vel * ${DT};

    let sampledValue = sampleBilinear(prevPos);

    scalarOut[i] = sampledValue * ${DECAY};
}
`;

const addScalarSourceShaderCode = `
@binding(0) @group(0) var<uniform> sourceParams: vec4f;
@binding(1) @group(0) var<storage, read> scalarIn: array<f32>;
@binding(2) @group(0) var<storage, read_write> scalarOut: array<f32>;

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
    let contribution = magnitude * exp(-dist2 / (radius * radius));

    scalarOut[i] = scalarIn[i] + contribution;
}
`;

async function initAdvectionSemiLagrangian() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);

  let depthTexture = createDepthTexture(device, canvas);

  const { xzBuffer, indices, indexBuffer } = gridBuffers(
    device,
    GRID_SIZE,
    GRID_SPACING,
  );

  const scalarBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  device.queue.writeBuffer(scalarBuffers[0], 0, new Float32Array(CELL_COUNT));

  const velocityData = new Float32Array(CELL_COUNT * 2);
  const offset = (GRID_SIZE - 1) * GRID_SPACING * 0.5;

  for (let y = 0; y < GRID_SIZE; y++) {
    for (let x = 0; x < GRID_SIZE; x++) {
      const worldX = x * GRID_SPACING - offset;
      const worldY = y * GRID_SPACING - offset;
      const r = Math.sqrt(worldX * worldX + worldY * worldY);

      if (r > 0.0001) {
        const vx = (-worldY / r) * ANGULAR_VELOCITY;
        const vy = (worldX / r) * ANGULAR_VELOCITY;

        const idx = (y * GRID_SIZE + x) * 2;
        velocityData[idx] = vx;
        velocityData[idx + 1] = vy;
      }
    }
  }

  const velocityBuffer = createStorageBuffer(device, velocityData.byteLength);
  device.queue.writeBuffer(velocityBuffer, 0, velocityData);

  const mvpBuffer = createUniformBuffer(device, 64);
  const sourceParamsBuffer = createUniformBuffer(device, 16);

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    fragmentShaderCode,
    format,
  );
  const advection = create_compute_pipeline(device, advectionComputeShaderCode);
  const source = create_compute_pipeline(device, addScalarSourceShaderCode);

  let step = 0;
  let frameCount = 0;

  function addScalarBlob() {
    const randomX = Math.random() * GRID_SIZE;
    const randomY = Math.random() * GRID_SIZE;
    const radius = 10.0;
    const magnitude = 1.0;

    writeBuffer(
      device,
      sourceParamsBuffer,
      new Float32Array([randomX, randomY, magnitude, radius]),
    );

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        source,
        [
          sourceParamsBuffer,
          scalarBuffers[step % 2],
          scalarBuffers[(step + 1) % 2],
        ],
        WORKGROUP_COUNT,
      );
    });
    step++;
  }

  const camera = createOrbitCamera({
    radius: 20.0,
    height: 4.0,
    speed: 0.0002,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.05, g: 0.05, b: 0.1, a: 1.0 },
  });

  addScalarBlob();

  function render() {
    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

    frameCount++;

    if (frameCount % ADVECTION_SPAWN_INTERVAL === 0) {
      addScalarBlob();
    }

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        advection,
        [
          scalarBuffers[step % 2],
          scalarBuffers[(step + 1) % 2],
          velocityBuffer,
        ],
        WORKGROUP_COUNT,
      );
    });
    step++;

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
        [mvpBuffer, scalarBuffers[step % 2]],
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

initAdvectionSemiLagrangian();