/*
Hydraulic Erosion Simulation: 

This simulation generates procedural terrain using fractal noise and then erodes it through water flow. The terrain starts as multi-octave noise with ridge features mixed in. Water droplets are spawned randomly across the surface, and a simplified fluid simulation tracks water depth, velocity, and sediment at each grid cell. Water flows downhill based on height differences between neighboring cells, picking up sediment when moving fast (erosion) and depositing it when moving slowly. Evaporation gradually removes water and sediment over time. The erosion runs for a fixed number of iterations at startup to carve realistic valleys and drainage patterns. The visualization colors the terrain based on height, from deep blue for low areas through tan and green for mid-elevations to white for peaks, with lighting based on surface slope.
*/

const GRID_SIZE = 500;
const GRID_SPACING = 0.05;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;

const EROSION_ITERATIONS = 2000;
const DROPLETS_PER_FRAME = 50;
const NOISE_SEED = Math.random() * 1000;
const NOISE_SCALE = 8.0;
const NOISE_OCTAVES = 5;
const EROSION_DT = 0.1;
const EROSION_GRAVITY = 9.8;
const EROSION_EVAPORATION = 0.01;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> heightField: array<f32>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) height: f32,
    @location(1) worldPos: vec3<f32>,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let h = heightField[input.vertexIndex];
    let pos = vec3<f32>(input.xz.x, h, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.height = h;
    output.worldPos = pos;
    return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) height: f32, @location(1) worldPos: vec3<f32>) -> @location(0) vec4<f32> {
    let h = clamp(height, -2.0, 2.0);

    var color: vec3<f32>;
    if (h < -0.5) {
        color = vec3<f32>(0.1, 0.2, 0.4);
    } else if (h < -0.1) {
        color = mix(vec3<f32>(0.1, 0.2, 0.4), vec3<f32>(0.2, 0.3, 0.5), (h + 0.5) * 2.0);
    } else if (h < 0.3) {
        color = mix(vec3<f32>(0.8, 0.7, 0.5), vec3<f32>(0.3, 0.5, 0.2), h * 3.33);
    } else if (h < 1.0) {
        color = mix(vec3<f32>(0.3, 0.5, 0.2), vec3<f32>(0.4, 0.4, 0.3), (h - 0.3) * 1.43);
    } else {
        color = mix(vec3<f32>(0.4, 0.4, 0.3), vec3<f32>(0.9, 0.9, 0.95), (h - 1.0));
    }

    let dx = dpdx(worldPos).y;
    let dz = dpdy(worldPos).y;
    let slope = sqrt(dx * dx + dz * dz);
    let lighting = 1.0 - slope * 0.5;

    return vec4<f32>(color * lighting, 1.0);
}
`;

const noiseGenShaderCode = `
@binding(0) @group(0) var<storage, read_write> heightField: array<f32>;

fn hash2(p: vec2f) -> f32 {
    let k = vec2f(0.3183099, 0.3678794);
    let p2 = p + k * ${NOISE_SEED};
    return fract(sin(dot(p2, vec2f(127.1, 311.7))) * 43758.5453);
}

fn noise2d(p: vec2f) -> f32 {
    let i = floor(p);
    let f = fract(p);

    let a = hash2(i);
    let b = hash2(i + vec2f(1.0, 0.0));
    let c = hash2(i + vec2f(0.0, 1.0));
    let d = hash2(i + vec2f(1.0, 1.0));

    let u = f * f * (3.0 - 2.0 * f);

    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

fn fbm(p: vec2f, octaves: i32) -> f32 {
    var value = 0.0;
    var amplitude = 0.5;
    var frequency = 1.0;
    var maxValue = 0.0;

    for (var i = 0; i < octaves; i++) {
        value += amplitude * noise2d(p * frequency);
        maxValue += amplitude;
        amplitude *= 0.5;
        frequency *= 2.0;
    }

    return value / maxValue;
}

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let scale = f32(${NOISE_SCALE});
    let octaves = ${NOISE_OCTAVES};

    let pos = vec2f(f32(cell.x), f32(cell.y)) * scale / f32(${GRID_SIZE});

    var height = fbm(pos, octaves);

    let ridgeNoise = 1.0 - abs(noise2d(pos * 3.0) * 2.0 - 1.0);
    height = mix(height, ridgeNoise, 0.3);

    height = (height - 0.5) * 4.0;

    heightField[cellIndex(cell.x, cell.y)] = height;
}
`;

const erosionShaderCode = `
@binding(0) @group(0) var<storage, read> heightIn: array<f32>;
@binding(1) @group(0) var<storage, read_write> heightOut: array<f32>;
@binding(2) @group(0) var<storage, read_write> waterFlow: array<vec4f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

fn getHeight(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return 999.0;
    }
    return heightIn[cellIndex(u32(x), u32(y))];
}

fn getWater(x: i32, y: i32) -> vec4f {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return vec4f(0.0);
    }
    return waterFlow[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let idx = cellIndex(cell.x, cell.y);

    let dt = ${EROSION_DT};
    let gravity = ${EROSION_GRAVITY};
    let evaporation = ${EROSION_EVAPORATION};

    let h = getHeight(x, y);
    var water = getWater(x, y);

    let h_n = getHeight(x, y - 1);
    let h_s = getHeight(x, y + 1);
    let h_e = getHeight(x + 1, y);
    let h_w = getHeight(x - 1, y);

    let total_h = h + water.z;

    let flow_n = max(0.0, total_h - (h_n + getWater(x, y - 1).z));
    let flow_s = max(0.0, total_h - (h_s + getWater(x, y + 1).z));
    let flow_e = max(0.0, total_h - (h_e + getWater(x + 1, y).z));
    let flow_w = max(0.0, total_h - (h_w + getWater(x - 1, y).z));

    let total_flow = flow_n + flow_s + flow_e + flow_w;

    if (total_flow > 0.0001) {
        let scale = min(1.0, water.z / (total_flow + 0.0001));
        water.x = (flow_e - flow_w) * scale * 10.0;
        water.y = (flow_s - flow_n) * scale * 10.0;
    }

    water.x *= 0.9;
    water.y *= 0.9;

    let speed = length(water.xy);
    let erosionRate = speed * 0.001;
    let depositionRate = 0.001;

    let capacity = speed * water.z * 0.1;

    var heightDelta = 0.0;

    if (water.w < capacity) {
        let erodeAmount = min(erosionRate * dt, h + 1.0);
        heightDelta = -erodeAmount;
        water.w += erodeAmount;
    } else {
        let depositAmount = min((water.w - capacity) * depositionRate * dt, water.w);
        heightDelta = depositAmount;
        water.w -= depositAmount;
    }

    water.z *= (1.0 - evaporation * dt);
    water.w *= (1.0 - evaporation * dt * 0.5);

    heightOut[idx] = h + heightDelta;
    waterFlow[idx] = water;
}
`;

const dropletShaderCode = `
@binding(0) @group(0) var<uniform> dropletParams: vec2f;
@binding(1) @group(0) var<storage, read_write> waterFlow: array<vec4f>;

fn hash(p: vec2f) -> f32 {
    return fract(sin(dot(p, vec2f(127.1, 311.7))) * 43758.5453);
}

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) id: vec3u) {
    if (id.x >= ${DROPLETS_PER_FRAME}u) {
        return;
    }

    let seed = dropletParams.x + dropletParams.y;

    let rand1 = hash(vec2f(f32(id.x) * 1.1, seed));
    let rand2 = hash(vec2f(f32(id.x) * 2.3, seed + 1.0));

    let x = u32(rand1 * f32(${GRID_SIZE} - 1));
    let y = u32(rand2 * f32(${GRID_SIZE} - 1));

    let idx = cellIndex(x, y);

    waterFlow[idx].z += 0.1;
}
`;

async function initTerrainSim() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);

  let depthTexture = createDepthTexture(device);

  const { xzBuffer, indices, indexBuffer } = gridBuffers(
    device,
    GRID_SIZE,
    GRID_SPACING,
  );

  const heightBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  const waterFlowBuffer = createStorageBuffer(device, CELL_COUNT * 16);

  const mvpBuffer = createUniformBuffer(device, 64);
  const dropletParamsBuffer = createUniformBuffer(device, 8);

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    fragmentShaderCode,
    format,
    "triangle-list",
  );

  const noiseGenPipeline = create_compute_pipeline(device, noiseGenShaderCode);

  const erosionPipeline = create_compute_pipeline(device, erosionShaderCode);

  const dropletPipeline = create_compute_pipeline(device, dropletShaderCode);

  let step = 0;
  let time = 0;
  let terrainGenerated = false;

  function generateTerrain() {
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        noiseGenPipeline,
        [heightBuffers[0]],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        noiseGenPipeline,
        [heightBuffers[1]],
        WORKGROUP_COUNT,
      );
    });
  }

  function simulateErosion() {
    if (step % 2 === 0) {
      device.queue.writeBuffer(
        dropletParamsBuffer,
        0,
        new Float32Array([Math.random() * 100, time]),
      );

      submit_commands(device, (encoder) => {
        dispatch(
          device,
          encoder,
          dropletPipeline,
          [dropletParamsBuffer, waterFlowBuffer],
          1,
        );
      });
    }

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        erosionPipeline,
        [
          heightBuffers[step % 2],
          heightBuffers[(step + 1) % 2],
          waterFlowBuffer,
        ],
        WORKGROUP_COUNT,
      );
    });
    step++;
  }

  const camera = createOrbitCamera({
    radius: 20.0,
    height: 12.0,
    speed: 0.0001,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.5, g: 0.7, b: 0.9, a: 1.0 },
  });

  function render() {
    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    if (!terrainGenerated) {
      generateTerrain();
      terrainGenerated = true;
    }

    if (step < EROSION_ITERATIONS) {
      for (let i = 0; i < 5; i++) {
        simulateErosion();
      }
    }

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

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

    time += 0.016;
    requestAnimationFrame(render);
  }

  render();
}

initTerrainSim();