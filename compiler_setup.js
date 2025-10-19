var container;
var controlsContainer;
var sourcePanel = null;
var outputPanels = [];
var sourceContent = "";
var stages = [
    { name: "Source", enabled: true },
    { name: "AST", enabled: false },
    { name: "Lowered Source", enabled: false },
    { name: "Lowered AST", enabled: false },
    { name: "Assembly", enabled: false },
    { name: "SSA", enabled: true },
    { name: "Optimized SSA", enabled: false },
    { name: "Optimized Assembly", enabled: true }
];

function init() {
    container = document.querySelector(".panels");
    if (!container) {
        throw new Error("Panels container not found");
    }

    controlsContainer = document.createElement("div");
    controlsContainer.className = "controls";

    document.body.appendChild(controlsContainer);

    createControls();
}

function createControls() {
    const programSelect = document.createElement("select");
    programSelect.className = "program-select";

    for (const programName of Object.keys(testPrograms)) {
        const option = document.createElement("option");
        option.value = programName;
        option.textContent = programName;
        programSelect.appendChild(option);
    }
    programSelect.onchange = (e) => {
        const target = e.target;
        loadProgram(target.value);
    };
    controlsContainer.appendChild(programSelect);

    const checkboxContainer = document.createElement("div");
    checkboxContainer.className = "checkbox-container";

    for (let i = 0; i < stages.length; i++) {
        const stage = stages[i];
        const label = document.createElement("label");

        const checkbox = document.createElement("input");
        checkbox.type = "checkbox";
        checkbox.checked = stage.enabled;
        checkbox.onchange = () => {
            stages[i].enabled = checkbox.checked;
            compile(sourceContent);
        };
        label.appendChild(checkbox);
        label.appendChild(document.createTextNode(stage.name));
        checkboxContainer.appendChild(label);
    }
    controlsContainer.appendChild(checkboxContainer);
}

function clearPanels() {
    if (sourcePanel) {
        sourcePanel.container.remove();
        sourcePanel = null;
    }
    outputPanels.forEach((output) => output.container.remove());
    outputPanels = [];
}

function loadProgram(programName) {
    clearPanels();
    const program = testPrograms[programName];
    if (program) {
        sourceContent = program;
        compile(program);
    }
}

function createPanel(label, readonly) {
    const panel = document.createElement("div");
    panel.className = "panel";
    const header = document.createElement("div");
    header.className = "panel-header";
    const labelEl = document.createElement("div");
    labelEl.className = "panel-label";
    labelEl.textContent = label;
    header.appendChild(labelEl);
    const textarea = document.createElement("textarea");
    textarea.spellcheck = false;
    textarea.readOnly = readonly;
    panel.appendChild(header);
    panel.appendChild(textarea);
    container.appendChild(panel);
    return { container: panel, label: labelEl, textarea };
}

function setOutput(stageName, content) {
    const stageIndex = stages.findIndex((s) => s.name === stageName);
    if (stageIndex === -1 || !stages[stageIndex].enabled) {
        return;
    }
    if (stageName === "Source" && !sourcePanel) {
        sourcePanel = createPanel("Source", false);
        sourcePanel.textarea.value = sourceContent;
        sourcePanel.textarea.oninput = () => {
            sourceContent = sourcePanel.textarea.value;
            compile(sourceContent);
        };
        return;
    }
    const panel = createPanel(stageName, true);
    panel.textarea.value = content;
    outputPanels.push(panel);
}

function compile(source) {
    clearPanels();
    sourceContent = source;
    setOutput("Source", source);
    const parseResult = parse(source);
    if (parseResult.error !== "") {
        const errorPanel = createPanel("Parse Error", true);
        errorPanel.textarea.value = parseResult.error;
        outputPanels.push(errorPanel);
        return;
    }
    const ast = parseResult.ast;
    setOutput("AST", astPrint(ast));
    const semanticResult = semantic_analyze(ast);
    if (semanticResult.error) {
        const errorPanel = createPanel("Semantic Error", true);
        errorPanel.textarea.value = semanticResult.error;
        outputPanels.push(errorPanel);
        return;
    }
    const lowered = normalizeProgram(ast, semanticResult.symbolTable);
    setOutput("Lowered Source", emitC(lowered.program));
    setOutput("Lowered AST", astPrint(lowered.program));
    const ir = generateIR(lowered.program, lowered.symbolTable);
    const unoptimizedIR = structuredClone(ir);
    const unoptimizedAllocations = allocateRegisters(unoptimizedIR);
    setOutput(
        "Assembly",
        emitProgram(unoptimizedIR, unoptimizedAllocations)
    );
    constructSSA(ir);
    setOutput("SSA", printIr(ir));
    optimizeSSA(ir);
    setOutput("Optimized SSA", printIr(ir));
    destructSSA(ir);
    postSSA(ir);
    constructSSA(ir);
    optimizeSSA(ir);
    destructSSA(ir);
    postSSA(ir);
    const allocations = allocateRegisters(ir);
    setOutput("Optimized Assembly", emitProgram(ir, allocations));
}

init();
loadProgram("sccp_test");