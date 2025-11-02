const GRID_SIZE = 120;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;

const WAVE_SPAWN_INTERVAL = 100;
const DT = 0.04;
const WAVE_SPEED = 1.0;
const DAMPING = 0.998;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> height: array<f32>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) h: f32,
    @location(1) distance: f32,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let h = height[input.vertexIndex];
    let y = h;
    let pos = vec3<f32>(input.xz.x, y, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.h = h;
    output.distance = length(pos);
    return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) h: f32, @location(1) distance: f32) -> @location(0) vec4<f32> {
    let intensity = clamp(abs(h) * 0.9, 0.0, 1.0);
    let color = vec3<f32>(intensity, intensity, intensity);

    let fadeStart = 7.0;
    let fadeEnd = 9.0;
    let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);

    return vec4<f32>(0.3, 0.3, 0.3, alpha);
}
`;

const computeDerivativeShaderCode = `
@binding(0) @group(0) var<storage, read> height: array<f32>;
@binding(1) @group(0) var<storage, read> velocity: array<f32>;
@binding(2) @group(0) var<storage, read_write> k_out: array<vec2f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getHeight(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return 0.0;
    }
    return height[cellIndex(u32(x), u32(y))];
}

fn laplacian9Point(x: i32, y: i32) -> f32 {
    let c = getHeight(x, y);
    let n = getHeight(x, y + 1);
    let s = getHeight(x, y - 1);
    let e = getHeight(x + 1, y);
    let w = getHeight(x - 1, y);
    let ne = getHeight(x + 1, y + 1);
    let nw = getHeight(x - 1, y + 1);
    let se = getHeight(x + 1, y - 1);
    let sw = getHeight(x - 1, y - 1);

    let dx = f32(${GRID_SPACING});
    return (4.0*(n + s + e + w) + (ne + nw + se + sw) - 20.0*c) / (6.0 * dx * dx);
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let c2 = f32(${WAVE_SPEED}) * f32(${WAVE_SPEED});

    let k_h = velocity[i];
    let k_v = c2 * laplacian9Point(x, y);

    k_out[i] = vec2f(k_h, k_v);
}
`;

const buildIntermediateShaderCode = `
@binding(0) @group(0) var<storage, read> h_base: array<f32>;
@binding(1) @group(0) var<storage, read> v_base: array<f32>;
@binding(2) @group(0) var<storage, read> k: array<vec2f>;
@binding(3) @group(0) var<uniform> factor: f32;
@binding(4) @group(0) var<storage, read_write> h_out: array<f32>;
@binding(5) @group(0) var<storage, read_write> v_out: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let i = cellIndex(cell.x, cell.y);
    let dt = f32(${DT});
    let k_hv = k[i];

    h_out[i] = h_base[i] + factor * dt * k_hv.x;
    v_out[i] = v_base[i] + factor * dt * k_hv.y;
}
`;

const rk4CombineShaderCode = `
@binding(0) @group(0) var<storage, read> h_base: array<f32>;
@binding(1) @group(0) var<storage, read> v_base: array<f32>;
@binding(2) @group(0) var<storage, read> k1: array<vec2f>;
@binding(3) @group(0) var<storage, read> k2: array<vec2f>;
@binding(4) @group(0) var<storage, read> k3: array<vec2f>;
@binding(5) @group(0) var<storage, read> k4: array<vec2f>;
@binding(6) @group(0) var<storage, read_write> h_out: array<f32>;
@binding(7) @group(0) var<storage, read_write> v_out: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);
    let dt = f32(${DT});

    let k1_hv = k1[i];
    let k2_hv = k2[i];
    let k3_hv = k3[i];
    let k4_hv = k4[i];

    var h_new = h_base[i] + (dt / 6.0) * (k1_hv.x + 2.0*k2_hv.x + 2.0*k3_hv.x + k4_hv.x);
    var v_new = v_base[i] + (dt / 6.0) * (k1_hv.y + 2.0*k2_hv.y + 2.0*k3_hv.y + k4_hv.y);

    h_new *= f32(${DAMPING});
    v_new *= f32(${DAMPING});

    let boundaryWidth = 2;
    if (x < boundaryWidth || x >= ${GRID_SIZE - 2} || y < boundaryWidth || y >= ${GRID_SIZE - 2}) {
        h_new *= 0.9;
        v_new *= 0.9;
    }

    h_out[i] = h_new;
    v_out[i] = v_new;
}
`;

const addImpulseShaderCode = `
@binding(0) @group(0) var<uniform> impulseParams: vec4f;
@binding(1) @group(0) var<storage, read> heightIn: array<f32>;
@binding(2) @group(0) var<storage, read_write> heightOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let i = cellIndex(cell.x, cell.y);
    let centerX = impulseParams.x;
    let centerY = impulseParams.y;
    let magnitude = impulseParams.z;

    let dx = f32(cell.x) - centerX;
    let dy = f32(cell.y) - centerY;

    if (abs(dx) < 0.5 && abs(dy) < 0.5) {
        heightOut[i] = heightIn[i] + magnitude;
    } else {
        heightOut[i] = heightIn[i];
    }
}
`;

async function initWaveSim() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);

  let depthTexture = createDepthTexture(device, canvas);

  const { xzBuffer, indices, indexBuffer } = gridBuffers(
    device,
    GRID_SIZE,
    GRID_SPACING,
  );

  const heightBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  const velocityBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  const k1_buffer = createStorageBuffer(device, CELL_COUNT * 8);
  const k2_buffer = createStorageBuffer(device, CELL_COUNT * 8);
  const k3_buffer = createStorageBuffer(device, CELL_COUNT * 8);
  const k4_buffer = createStorageBuffer(device, CELL_COUNT * 8);

  const h_temp_buffer = createStorageBuffer(device, CELL_COUNT * 4);
  const v_temp_buffer = createStorageBuffer(device, CELL_COUNT * 4);

  device.queue.writeBuffer(heightBuffers[0], 0, new Float32Array(CELL_COUNT));
  device.queue.writeBuffer(velocityBuffers[0], 0, new Float32Array(CELL_COUNT));

  const mvpBuffer = createUniformBuffer(device, 64);
  const impulseParamsBuffer = createUniformBuffer(device, 16);
  const factorBuffer = createUniformBuffer(device, 4);

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    fragmentShaderCode,
    format,
  );

  const derivativePipeline = create_compute_pipeline(
    device,
    computeDerivativeShaderCode,
  );

  const intermediatePipeline = create_compute_pipeline(
    device,
    buildIntermediateShaderCode,
  );

  const combinePipeline = create_compute_pipeline(device, rk4CombineShaderCode);

  const impulsePipeline = create_compute_pipeline(device, addImpulseShaderCode);

  let step = 0;
  let frameCount = 0;

  function addImpulse() {
    const randomX = Math.random() * GRID_SIZE;
    const randomY = Math.random() * GRID_SIZE;
    const magnitude = 2.0;

    writeBuffer(
      device,
      impulseParamsBuffer,
      new Float32Array([randomX, randomY, magnitude, 0]),
    );

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        impulsePipeline,
        [
          impulseParamsBuffer,
          heightBuffers[step % 2],
          heightBuffers[(step + 1) % 2],
        ],
        WORKGROUP_COUNT,
      );
    });
    step++;
  }

  const camera = createOrbitCamera({
    radius: 10.0,
    height: 3.0,
    speed: 0.0002,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.1, g: 0.1, b: 0.1, a: 1.0 },
  });

  addImpulse();

  function performRK4Step() {
    const h_in = heightBuffers[step % 2];
    const v_in = velocityBuffers[step % 2];
    const h_out = heightBuffers[(step + 1) % 2];
    const v_out = velocityBuffers[(step + 1) % 2];

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        derivativePipeline,
        [h_in, v_in, k1_buffer],
        WORKGROUP_COUNT,
      );
    });

    writeBuffer(device, factorBuffer, new Float32Array([0.5]));
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        intermediatePipeline,
        [h_in, v_in, k1_buffer, factorBuffer, h_temp_buffer, v_temp_buffer],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        derivativePipeline,
        [h_temp_buffer, v_temp_buffer, k2_buffer],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        intermediatePipeline,
        [h_in, v_in, k2_buffer, factorBuffer, h_temp_buffer, v_temp_buffer],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        derivativePipeline,
        [h_temp_buffer, v_temp_buffer, k3_buffer],
        WORKGROUP_COUNT,
      );
    });

    writeBuffer(device, factorBuffer, new Float32Array([1.0]));
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        intermediatePipeline,
        [h_in, v_in, k3_buffer, factorBuffer, h_temp_buffer, v_temp_buffer],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        derivativePipeline,
        [h_temp_buffer, v_temp_buffer, k4_buffer],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        combinePipeline,
        [h_in, v_in, k1_buffer, k2_buffer, k3_buffer, k4_buffer, h_out, v_out],
        WORKGROUP_COUNT,
      );
    });

    step++;
  }

  function render() {
    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

    frameCount++;

    if (frameCount % WAVE_SPAWN_INTERVAL === 0) {
      addImpulse();
    }

    performRK4Step();

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
        [mvpBuffer, heightBuffers[step % 2]],
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

initWaveSim();