function createCodeEditor() {
  const panel = document.createElement("div");
  panel.id = "code-editor-panel";
  panel.style.cssText = `
    position: fixed;
    right: 20px;
    top: 20px;
    width: 600px;
    height: 700px;
    background: #1e1e1e;
    border: 1px solid #3e3e3e;
    border-radius: 4px;
    display: flex;
    flex-direction: column;
    box-shadow: 0 4px 12px rgba(0,0,0,0.5);
    z-index: 1000;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  `;

  const header = document.createElement("div");
  header.style.cssText = `
    background: #2d2d2d;
    padding: 8px 12px;
    cursor: move;
    user-select: none;
    border-bottom: 1px solid #3e3e3e;
    display: flex;
    align-items: center;
    justify-content: space-between;
  `;

  const titleContainer = document.createElement("div");
  titleContainer.style.cssText =
    "display: flex; align-items: center; gap: 8px;";

  const title = document.createElement("span");
  title.textContent = "Editor";
  title.style.cssText = "color: #cccccc; font-size: 13px; font-weight: 500;";

  const filenameDisplay = document.createElement("span");
  filenameDisplay.style.cssText =
    "color: #888; font-size: 11px; font-family: monospace;";

  titleContainer.appendChild(title);
  titleContainer.appendChild(filenameDisplay);

  const closeBtn = document.createElement("button");
  closeBtn.textContent = "×";
  closeBtn.style.cssText = `
    background: none;
    border: none;
    color: #cccccc;
    font-size: 20px;
    cursor: pointer;
    padding: 0;
    width: 20px;
    height: 20px;
    line-height: 1;
  `;
  closeBtn.onclick = () => {
    panel.style.display = "none";
  };

  header.appendChild(titleContainer);
  header.appendChild(closeBtn);

  const selectorContainer = document.createElement("div");
  selectorContainer.style.cssText = `
    padding: 8px 12px;
    background: #252525;
    border-bottom: 1px solid #3e3e3e;
    display: flex;
    gap: 8px;
    align-items: center;
  `;

  const selectLabel = document.createElement("label");
  selectLabel.textContent = "File:";
  selectLabel.style.cssText = "color: #cccccc; font-size: 12px;";

  const select = document.createElement("select");
  select.id = "file-selector";
  select.style.cssText = `
    flex: 1;
    padding: 6px 8px;
    background: #3c3c3c;
    border: 1px solid #555;
    border-radius: 3px;
    color: #cccccc;
    font-size: 12px;
    cursor: pointer;
  `;

  selectorContainer.appendChild(selectLabel);
  selectorContainer.appendChild(select);

  const editorContainer = document.createElement("div");
  editorContainer.style.cssText = `
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
  `;

  const textarea = document.createElement("textarea");
  textarea.id = "simulation-code";
  textarea.style.cssText = `
    flex: 1;
    width: 98%;
    padding-left: 12px;
    background: #1e1e1e;
    color: #d4d4d4;
    border: none;
    font-family: 'Courier New', Consolas, monospace;
    font-size: 12px;
    line-height: 1.5;
    resize: none;
    outline: none;
    tab-size: 4;
    text-wrap: wrap;
  `;
  textarea.spellcheck = false;

  textarea.addEventListener("keydown", (e) => {
    if (e.key === "Tab") {
      e.preventDefault();
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      textarea.value =
        textarea.value.substring(0, start) +
        "    " +
        textarea.value.substring(end);
      textarea.selectionStart = textarea.selectionEnd = start + 4;
    }

    if ((e.ctrlKey || e.metaKey) && e.key === "s") {
      e.preventDefault();
      reloadBtn.click();
    }
  });

  editorContainer.appendChild(textarea);

  const errorDisplay = document.createElement("div");
  errorDisplay.id = "code-error";
  errorDisplay.style.cssText = `
    max-height: 150px;
    overflow-y: auto;
    padding: 8px 12px;
    background: #3c1f1f;
    color: #f48771;
    font-family: 'Courier New', Consolas, monospace;
    font-size: 11px;
    border-top: 1px solid #5a2d2d;
    display: none;
    white-space: pre-wrap;
  `;

  const footer = document.createElement("div");
  footer.style.cssText = `
    padding: 12px;
    background: #252525;
    border-top: 1px solid #3e3e3e;
    display: flex;
    gap: 8px;
    align-items: center;
    text-wrap: wrap;
  `;

  const reloadBtn = document.createElement("button");
  reloadBtn.textContent = "Reload";
  reloadBtn.style.cssText = `
    flex: 1;
    padding: 8px 16px;
    background: #0e639c;
    color: white;
    border: none;
    border-radius: 3px;
    cursor: pointer;
    font-size: 13px;
    font-weight: 500;
  `;
  reloadBtn.onmouseenter = () => {
    reloadBtn.style.background = "#1177bb";
  };
  reloadBtn.onmouseleave = () => {
    reloadBtn.style.background = "#0e639c";
  };

  const statusText = document.createElement("span");
  statusText.style.cssText = `
    color: #888;
    font-size: 11px;
    font-family: monospace;
  `;

  footer.appendChild(reloadBtn);
  footer.appendChild(statusText);

  panel.appendChild(header);
  panel.appendChild(selectorContainer);
  panel.appendChild(editorContainer);
  panel.appendChild(errorDisplay);
  panel.appendChild(footer);

  document.body.appendChild(panel);

  let isDragging = false;
  let currentX;
  let currentY;
  let initialX;
  let initialY;

  header.addEventListener("mousedown", (e) => {
    if (e.target === closeBtn) return;
    isDragging = true;
    initialX = e.clientX - panel.offsetLeft;
    initialY = e.clientY - panel.offsetTop;
  });

  document.addEventListener("mousemove", (e) => {
    if (isDragging) {
      e.preventDefault();
      currentX = e.clientX - initialX;
      currentY = e.clientY - initialY;
      panel.style.left = currentX + "px";
      panel.style.top = currentY + "px";
      panel.style.right = "auto";
    }
  });

  document.addEventListener("mouseup", () => {
    isDragging = false;
  });

  let reloadCallback = null;
  let fileChangeCallback = null;

  return {
    setCode(code) {
      textarea.value = code;
      errorDisplay.style.display = "none";
      statusText.textContent = "";
    },

    getCode() {
      return textarea.value;
    },

    setFilename(name) {
      select.value = name;
    },

    setFiles(files) {
      select.innerHTML = "";
      files.forEach((name) => {
        const option = document.createElement("option");
        option.value = name;
        option.textContent = name;
        select.appendChild(option);
      });

      select.onchange = () => {
        if (fileChangeCallback) {
          fileChangeCallback(select.value);
        }
      };
    },

    showError(message) {
      errorDisplay.textContent = message;
      errorDisplay.style.display = "block";
      statusText.textContent = "❌ Error";
      statusText.style.color = "#f48771";
    },

    clearError() {
      errorDisplay.style.display = "none";
      statusText.textContent = "✓ Running";
      statusText.style.color = "#4ec9b0";

      setTimeout(() => {
        statusText.textContent = "";
      }, 2000);
    },

    onReload(callback) {
      reloadCallback = callback;
      reloadBtn.onclick = async () => {
        statusText.textContent = "⟳ Reloading...";
        statusText.style.color = "#cccccc";
        try {
          await callback(textarea.value);
        } catch (e) {

        }
      };
    },

    onFileChange(callback) {
      fileChangeCallback = callback;
    },

    show() {
      panel.style.display = "flex";

    },

    hide() {
      panel.style.display = "none";
    },
  };
}