import re
import json
from dataclasses import dataclass
from typing import List, Optional
from enum import Enum

class BufferType(Enum):
    UNIFORM = "uniform"
    STORAGE_READ = "read-only-storage"
    STORAGE_WRITE = "storage"
    
class ShaderStage(Enum):
    VERTEX = "vertex"
    FRAGMENT = "fragment"
    COMPUTE = "compute"

@dataclass
class WGSLType:
    base_type: str
    is_array: bool
    
    def byte_size(self):
        type_map = {
            'f32': 4,
            'u32': 4,
            'i32': 4,
            'vec2f': 8,
            'vec2<f32>': 8,
            'vec3f': 12,
            'vec3<f32>': 12,
            'vec4f': 16,
            'vec4<f32>': 16,
            'mat4x4<f32>': 64,
        }
        return type_map.get(self.base_type, 16)

@dataclass
class VertexAttribute:
    location: int
    type_str: str
    name: str
    format: str

@dataclass
class Binding:
    binding_index: int
    group_index: int
    buffer_type: BufferType
    variable_name: str
    wgsl_type: WGSLType
    shader_stage: ShaderStage

@dataclass
class ShaderInfo:
    filename: str
    stage: ShaderStage
    bindings: List[Binding]
    vertex_attributes: List[VertexAttribute]
    workgroup_size: Optional[tuple] = None

@dataclass
class PipelineGroup:
    name: str
    shaders: List[ShaderInfo]
    is_compute: bool

def parse_wgsl_type(type_str):
    type_str = type_str.strip()
    array_match = re.match(r'array<(.+)>', type_str)
    if array_match:
        return WGSLType(base_type=array_match.group(1).strip(), is_array=True)
    return WGSLType(base_type=type_str, is_array=False)

def wgsl_type_to_vertex_format(type_str):
    type_map = {
        'f32': 'float32',
        'vec2<f32>': 'float32x2',
        'vec2f': 'float32x2',
        'vec3<f32>': 'float32x3',
        'vec3f': 'float32x3',
        'vec4<f32>': 'float32x4',
        'vec4f': 'float32x4',
    }
    return type_map.get(type_str, 'float32x4')

def infer_buffer_type(var_qualifier):
    var_qualifier = var_qualifier.strip()
    if var_qualifier.startswith('uniform'):
        return BufferType.UNIFORM
    elif 'read_write' in var_qualifier:
        return BufferType.STORAGE_WRITE
    elif var_qualifier.startswith('storage'):
        if ', read' in var_qualifier or ',read' in var_qualifier:
            return BufferType.STORAGE_READ
        return BufferType.STORAGE_READ
    return BufferType.UNIFORM

def parse_shader_stage(shader_code):
    if '@vertex' in shader_code:
        return ShaderStage.VERTEX
    elif '@fragment' in shader_code:
        return ShaderStage.FRAGMENT
    elif '@compute' in shader_code:
        return ShaderStage.COMPUTE
    return None

def parse_workgroup_size(shader_code):
    match = re.search(r'@workgroup_size\((\d+)(?:,\s*(\d+))?(?:,\s*(\d+))?\)', shader_code)
    if match:
        x = int(match.group(1))
        y = int(match.group(2)) if match.group(2) else 1
        z = int(match.group(3)) if match.group(3) else 1
        return (x, y, z)
    return None

def parse_vertex_inputs(shader_code):
    attributes = []
    struct_match = re.search(r'struct\s+VertexInput\s*\{([^}]+)\}', shader_code, re.DOTALL)
    if not struct_match:
        return attributes
    
    struct_body = struct_match.group(1)
    location_pattern = r'@location\((\d+)\)\s+(\w+):\s*([^,\n]+)'
    for match in re.finditer(location_pattern, struct_body):
        location = int(match.group(1))
        name = match.group(2)
        type_str = match.group(3).strip().rstrip(',')
        
        format_str = wgsl_type_to_vertex_format(type_str)
        
        attributes.append(VertexAttribute(
            location=location,
            type_str=type_str,
            name=name,
            format=format_str
        ))
    
    return sorted(attributes, key=lambda a: a.location)

def parse_bindings(shader_code, shader_stage):
    bindings = []
    pattern = r'@binding\((\d+)\)\s+@group\((\d+)\)\s+var<([^>]+)>\s+(\w+)\s*:\s*([^;]+);'
    matches = re.finditer(pattern, shader_code)
    
    for match in matches:
        binding_idx = int(match.group(1))
        group_idx = int(match.group(2))
        var_qualifier = match.group(3)
        var_name = match.group(4)
        var_type = match.group(5)
        
        buffer_type = infer_buffer_type(var_qualifier)
        wgsl_type = parse_wgsl_type(var_type)
        
        binding = Binding(
            binding_index=binding_idx,
            group_index=group_idx,
            buffer_type=buffer_type,
            variable_name=var_name,
            wgsl_type=wgsl_type,
            shader_stage=shader_stage
        )
        bindings.append(binding)
    
    return bindings

def extract_shader_code_from_js(js_content):
    shaders = []
    pattern = r'const\s+(\w+ShaderCode)\s*=\s*`(.*?)`\s*;'
    matches = re.finditer(pattern, js_content, re.DOTALL)
    
    for match in matches:
        shader_name = match.group(1)
        shader_code = match.group(2)
        shaders.append((shader_name, shader_code))
    
    return shaders

def parse_js_file(filename):
    with open(filename, 'r') as f:
        content = f.read()
    return parse_js_file_content(content)

def parse_html_file(filename):
    with open(filename, 'r') as f:
        content = f.read()
    script_match = re.search(r'<script>(.*?)</script>', content, re.DOTALL)
    if not script_match:
        return []
    return parse_js_file_content(script_match.group(1))

def parse_js_file_content(content):
    shaders = extract_shader_code_from_js(content)
    shader_infos = []
    
    for shader_name, shader_code in shaders:
        stage = parse_shader_stage(shader_code)
        if not stage:
            continue
            
        bindings = parse_bindings(shader_code, stage)
        vertex_attributes = parse_vertex_inputs(shader_code) if stage == ShaderStage.VERTEX else []
        workgroup_size = parse_workgroup_size(shader_code) if stage == ShaderStage.COMPUTE else None
        
        info = ShaderInfo(
            filename=shader_name,
            stage=stage,
            bindings=bindings,
            vertex_attributes=vertex_attributes,
            workgroup_size=workgroup_size
        )
        shader_infos.append(info)
    
    return shader_infos

def group_shaders_into_pipelines(shader_infos):
    pipelines = []
    used = set()
    
    for i, shader in enumerate(shader_infos):
        if i in used:
            continue
            
        if shader.stage == ShaderStage.VERTEX:
            fragment_shader = None
            for j, other in enumerate(shader_infos):
                if j != i and j not in used and other.stage == ShaderStage.FRAGMENT:
                    fragment_shader = other
                    used.add(j)
                    break
            
            base_name = shader.filename.replace('ShaderCode', '').replace('vertex', '').replace('Vertex', '')
            if not base_name:
                base_name = 'render'
            
            shaders_list = [shader]
            if fragment_shader:
                shaders_list.append(fragment_shader)
            
            pipelines.append(PipelineGroup(
                name=base_name,
                shaders=shaders_list,
                is_compute=False
            ))
            used.add(i)
    
    for i, shader in enumerate(shader_infos):
        if i in used:
            continue
            
        if shader.stage == ShaderStage.COMPUTE:
            base_name = shader.filename.replace('ShaderCode', '').replace('Shader', '')
            
            pipelines.append(PipelineGroup(
                name=base_name,
                shaders=[shader],
                is_compute=True
            ))
            used.add(i)
    
    return pipelines

def capitalize_first(s):
    if not s:
        return s
    return s[0].upper() + s[1:]

def generate_bind_group_creation_code(pipeline):
    binding_map = {}
    
    for shader in pipeline.shaders:
        for binding in shader.bindings:
            key = (binding.group_index, binding.binding_index)
            if key not in binding_map:
                binding_map[key] = binding
    
    sorted_bindings = sorted(binding_map.values(), 
                            key=lambda x: (x.group_index, x.binding_index))
    
    code = []
    func_name = f'create{capitalize_first(pipeline.name)}BindGroup'
    param_names = ', '.join([b.variable_name for b in sorted_bindings])
    
    code.append(f'function {func_name}(device, layout, [{param_names}]) {{')
    code.append('    return device.createBindGroup({')
    code.append('        layout: layout,')
    code.append('        entries: [')
    
    for i, binding in enumerate(sorted_bindings):
        code.append('            {')
        code.append(f'                binding: {binding.binding_index},')
        code.append(f'                resource: {{ buffer: {binding.variable_name} }}')
        code.append('            },')
    
    code.append('        ]')
    code.append('    });')
    code.append('}')
    
    return '\n'.join(code)

def generate_pipeline_resources_code(pipeline):
    binding_map = {}
    
    for shader in pipeline.shaders:
        for binding in shader.bindings:
            key = (binding.group_index, binding.binding_index)
            
            if key not in binding_map:
                binding_map[key] = {
                    'binding_index': binding.binding_index,
                    'group_index': binding.group_index,
                    'buffer_type': binding.buffer_type,
                    'visibility': [],
                    'variable_name': binding.variable_name
                }
            
            stage_flag = {
                ShaderStage.VERTEX: 'GPUShaderStage.VERTEX',
                ShaderStage.FRAGMENT: 'GPUShaderStage.FRAGMENT',
                ShaderStage.COMPUTE: 'GPUShaderStage.COMPUTE'
            }[shader.stage]
            
            if stage_flag not in binding_map[key]['visibility']:
                binding_map[key]['visibility'].append(stage_flag)
    
    sorted_bindings = sorted(binding_map.values(), 
                            key=lambda x: (x['group_index'], x['binding_index']))
    
    buffer_type_map = {
        BufferType.UNIFORM: 'uniform',
        BufferType.STORAGE_READ: 'read-only-storage',
        BufferType.STORAGE_WRITE: 'storage'
    }
    
    code = []
    func_name = f'create{capitalize_first(pipeline.name)}PipelineResources'
    
    code.append(f'function {func_name}(device) {{')
    code.append('    const bindGroupLayout = device.createBindGroupLayout({')
    code.append('        entries: [')
    
    for binding in sorted_bindings:
        visibility = ' | '.join(binding['visibility'])
        code.append('            {')
        code.append(f'                binding: {binding["binding_index"]},')
        code.append(f'                visibility: {visibility},')
        code.append(f'                buffer: {{ type: \'{buffer_type_map[binding["buffer_type"]]}\' }}')
        code.append('            },')
    
    code.append('        ]')
    code.append('    });')
    code.append('')
    code.append('    const pipelineLayout = device.createPipelineLayout({')
    code.append('        bindGroupLayouts: [bindGroupLayout]')
    code.append('    });')
    code.append('')
    code.append('    return { bindGroupLayout, pipelineLayout };')
    code.append('}')
    
    return '\n'.join(code)

def generate_render_pipeline_code(pipeline):
    vertex_shader = next((s for s in pipeline.shaders if s.stage == ShaderStage.VERTEX), None)
    fragment_shader = next((s for s in pipeline.shaders if s.stage == ShaderStage.FRAGMENT), None)
    
    if not vertex_shader:
        return ''
    
    code = []
    func_name = f'create{capitalize_first(pipeline.name)}Pipeline'
    
    code.append(f'function {func_name}(device, resources, format) {{')
    code.append('    return device.createRenderPipeline({')
    code.append('        layout: resources.pipelineLayout,')
    code.append('        vertex: {')
    code.append(f'            module: device.createShaderModule({{ code: {vertex_shader.filename} }}),')
    code.append('            entryPoint: \'main\',')
    
    if vertex_shader.vertex_attributes:
        code.append('            buffers: [')
        for attr in vertex_shader.vertex_attributes:
            wgsl_type = parse_wgsl_type(attr.type_str)
            stride = wgsl_type.byte_size()
            code.append('                {')
            code.append(f'                    arrayStride: {stride},')
            code.append('                    attributes: [{')
            code.append(f'                        shaderLocation: {attr.location},')
            code.append('                        offset: 0,')
            code.append(f'                        format: \'{attr.format}\',')
            code.append('                    }],')
            code.append('                },')
        code.append('            ],')
    
    code.append('        },')
    
    if fragment_shader:
        code.append('        fragment: {')
        code.append(f'            module: device.createShaderModule({{ code: {fragment_shader.filename} }}),')
        code.append('            entryPoint: \'main\',')
        code.append('            targets: [{')
        code.append('                format: format,')
        code.append('            }],')
        code.append('        },')
    
    code.append('        primitive: {')
    code.append('            topology: \'triangle-list\',')
    code.append('            cullMode: \'none\',')
    code.append('        },')
    code.append('    });')
    code.append('}')
    
    return '\n'.join(code)

def generate_compute_pipeline_code(pipeline):
    compute_shader = pipeline.shaders[0]
    
    code = []
    func_name = f'create{capitalize_first(pipeline.name)}Pipeline'
    
    code.append(f'function {func_name}(device, resources) {{')
    code.append('    return device.createComputePipeline({')
    code.append('        layout: resources.pipelineLayout,')
    code.append('        compute: {')
    code.append(f'            module: device.createShaderModule({{ code: {compute_shader.filename} }}),')
    code.append('            entryPoint: \'main\',')
    code.append('        }')
    code.append('    });')
    code.append('}')
    
    return '\n'.join(code)

def generate_webgpu_code(shader_infos, output_file):
    code = []
    code.append('// Auto-generated WebGPU binding code')
    code.append('// Generated from shader bindings\n')
    
    pipelines = group_shaders_into_pipelines(shader_infos)
    
    for pipeline in pipelines:
        code.append(generate_pipeline_resources_code(pipeline))
        code.append('')
        
        if pipeline.is_compute:
            code.append(generate_compute_pipeline_code(pipeline))
        else:
            code.append(generate_render_pipeline_code(pipeline))
        code.append('')
        
        code.append(generate_bind_group_creation_code(pipeline))
        code.append('')
    
    with open(output_file, 'w') as f:
        f.write('\n'.join(code))

def serialize_shader_info(shader_infos):
    result = []
    for info in shader_infos:
        shader_dict = {
            'filename': info.filename,
            'stage': info.stage.value,
            'workgroup_size': info.workgroup_size,
            'vertex_attributes': [
                {
                    'location': attr.location,
                    'type': attr.type_str,
                    'name': attr.name,
                    'format': attr.format
                }
                for attr in info.vertex_attributes
            ],
            'bindings': []
        }
        
        for binding in info.bindings:
            binding_dict = {
                'binding_index': binding.binding_index,
                'group_index': binding.group_index,
                'buffer_type': binding.buffer_type.value,
                'variable_name': binding.variable_name,
                'wgsl_type': {
                    'base_type': binding.wgsl_type.base_type,
                    'is_array': binding.wgsl_type.is_array
                },
                'shader_stage': binding.shader_stage.value
            }
            shader_dict['bindings'].append(binding_dict)
        
        result.append(shader_dict)
    
    return result

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python parse_shaders.py <input_file.js|.html> [output_prefix]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_prefix = sys.argv[2] if len(sys.argv) > 2 else 'shader_output'
    
    if input_file.endswith('.html'):
        shader_infos = parse_html_file(input_file)
    elif input_file.endswith('.js'):
        shader_infos = parse_js_file(input_file)
    else:
        print("Unsupported file type. Use .js or .html")
        sys.exit(1)
    
    output = serialize_shader_info(shader_infos)
    json_file = f'{output_prefix}.json'
    
    with open(json_file, 'w') as f:
        json.dump(output, f, indent=2)
    
    js_file = f'{output_prefix}.js'
    generate_webgpu_code(shader_infos, js_file)
    
    print(f"Parsed {len(shader_infos)} shaders")
    print(f"JSON output written to {json_file}")
    print(f"WebGPU code written to {js_file}")
    
    for info in shader_infos:
        print(f"\n{info.filename} ({info.stage.value}):")
        if info.workgroup_size:
            print(f"  Workgroup size: {info.workgroup_size}")
        if info.vertex_attributes:
            print(f"  Vertex attributes:")
            for attr in info.vertex_attributes:
                wgsl_type = parse_wgsl_type(attr.type_str)
                print(f"    @location({attr.location}) {attr.name}: {attr.type_str} -> {attr.format} (stride: {wgsl_type.byte_size()})")
        for binding in info.bindings:
            array_str = "[]" if binding.wgsl_type.is_array else ""
            print(f"  @binding({binding.binding_index}) @group({binding.group_index}) "
                  f"{binding.buffer_type.value}: {binding.variable_name}: "
                  f"{binding.wgsl_type.base_type}{array_str}")