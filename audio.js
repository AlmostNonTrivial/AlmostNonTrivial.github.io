/********************************************************************
 * 1. PHONETIC NORMALIZER & SPOKEN MATH PARSER
 ********************************************************************/
const WORD_TO_NUM = {
  'zero': 0, 'oh': 0, 'nought': 0, 'nil': 0,
  'one': 1, 'won': 1,
  'two': 2, 'to': 2, 'too': 2,
  'three': 3, 'tree': 3,
  'four': 4, 'for': 4, 'fore': 4,
  'five': 5,
  'six': 6, 'sicks': 6,
  'seven': 7,
  'eight': 8, 'ate': 8,
  'nine': 9, 'nein': 9,
  'ten': 10,
  'eleven': 11,
  'twelve': 12,
  'thirteen': 13,
  'fourteen': 14,
  'fifteen': 15,
  'sixteen': 16,
  'seventeen': 17,
  'eighteen': 18,
  'nineteen': 19,
  'twenty': 20,
  'thirty': 30,
  'forty': 40, 'fourty': 40,
  'fifty': 50,
  'sixty': 60,
  'seventy': 70,
  'eighty': 80,
  'ninety': 90
};

const CANONICAL_WORDS = {
  // Conics
  'circle': 'circle',
  'ellipse': 'ellipse', 'lips': 'ellipse', 'clips': 'ellipse',
  'parabola': 'parabola',
  'hyperbola': 'hyperbola',
  // Trig functions
  'sine': 'sine', 'sign': 'sine', 'sin': 'sine',
  'cosine': 'cosine', 'cosign': 'cosine', 'co-sign': 'cosine', 'cos': 'cosine',
  'tangent': 'tangent', 'tan': 'tangent',
  'secant': 'secant', 'sec': 'secant',
  'cosecant': 'cosecant', 'csc': 'cosecant',
  'cotangent': 'cotangent', 'cot': 'cotangent',
  // Parity
  'even': 'even',
  'odd': 'odd',
  'neither': 'neither'
};

const BOOLEAN_WORDS = {
  'true': true, 'yes': true, 'correct': true, 'yeah': true, 'yep': true,
  'false': false, 'no': false, 'incorrect': false, 'nope': false, 'faults': false, 'falls': false
};

/**
 * Parses spoken number phrases or digit strings into an array of candidate integers.
 * Supports negative prefixes ("minus", "negative") and compound spoken numbers up to 1000.
 */
function extractIntegersFromUtterance(rawText) {
  const cleaned = rawText
    .toLowerCase()
    .replace(/,/g, '')
    .replace(/[^\w\s-]/g, ' ')
    .trim();

  if (!cleaned) return [];

  const candidates = new Set();

  // 1. Direct digit matches (including "-42" or "minus 42" or "negative 42")
  const digitRegex = /(?:(minus|negative)\s+)?(-?\d+)/g;
  let match;
  while ((match = digitRegex.exec(cleaned)) !== null) {
    let val = parseInt(match[2], 10);
    if (!Number.isNaN(val)) {
      if (match[1] && val > 0) val = -val;
      candidates.add(val);
    }
  }

  // 2. Spoken word sequence parser
  const tokens = cleaned.split(/\s+/);
  let sign = 1;
  let current = 0;
  let acc = 0;
  let hasNumberToken = false;

  const flush = () => {
    if (hasNumberToken) {
      candidates.add(sign * (acc + current));
    }
    sign = 1;
    current = 0;
    acc = 0;
    hasNumberToken = false;
  };

  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    if (t === 'minus' || t === 'negative') {
      if (hasNumberToken) flush();
      sign = -1;
      continue;
    }
    if (t === 'and') continue;

    if (Object.prototype.hasOwnProperty.call(WORD_TO_NUM, t)) {
      const val = WORD_TO_NUM[t];
      current += val;
      hasNumberToken = true;
      candidates.add(sign * (acc + current));
    } else if (t === 'hundred') {
      if (current === 0) current = 1;
      current *= 100;
      hasNumberToken = true;
      candidates.add(sign * (acc + current));
    } else if (t === 'thousand') {
      if (current === 0) current = 1;
      acc += current * 1000;
      current = 0;
      hasNumberToken = true;
      candidates.add(sign * acc);
    } else if (/^-?\d+$/.test(t)) {
      const dVal = parseInt(t, 10);
      candidates.add(sign < 0 && dVal > 0 ? -dVal : dVal);
      flush();
    } else {
      flush();
    }
  }
  flush();

  return Array.from(candidates);
}

/**
 * Checks whether a spoken or typed string matches the active problem's expected answer
 * or triggers a voice command ("skip", "reveal").
 */
function evaluateUtterance(text, activeQuestion) {
  const lower = text.toLowerCase().trim();
  if (!lower) return { type: 'NONE' };

  const words = lower.replace(/[^\w\s-]/g, ' ').split(/\s+/).filter(Boolean);

  // Check voice commands first on recent words
  const recentWords = words.slice(-4);
  if (recentWords.some(w => w === 'skip' || w === 'pass' || w === 'next')) {
    return { type: 'COMMAND_SKIP' };
  }
  if (recentWords.some(w => w === 'reveal' || w === 'answer' || w === 'help')) {
    return { type: 'COMMAND_REVEAL' };
  }

  if (!activeQuestion) return { type: 'NONE' };

  // 1. INTEGER Answer Matching
  if (activeQuestion.answerType === 'INTEGER') {
    const nums = extractIntegersFromUtterance(lower);
    const latestNum = nums.length > 0 ? nums[nums.length - 1] : null;
    if (nums.includes(activeQuestion.answer)) {
      return {
        type: 'MATCH',
        matchedDisplay: String(activeQuestion.answer),
        preview: String(activeQuestion.answer)
      };
    }
    return {
      type: 'NO_MATCH',
      preview: latestNum !== null ? String(latestNum) : ''
    };
  }

  // 2. TRUE / FALSE Answer Matching
  if (activeQuestion.answerType === 'TRUE / FALSE') {
    let latestBool = null;
    for (const w of words) {
      if (Object.prototype.hasOwnProperty.call(BOOLEAN_WORDS, w)) {
        latestBool = BOOLEAN_WORDS[w];
        if (latestBool === activeQuestion.answer) {
          return {
            type: 'MATCH',
            matchedDisplay: latestBool ? 'True' : 'False',
            preview: latestBool ? 'True' : 'False'
          };
        }
      }
    }
    return {
      type: 'NO_MATCH',
      preview: latestBool !== null ? (latestBool ? 'True' : 'False') : ''
    };
  }

  // 3. WORD Answer Matching
  if (activeQuestion.answerType === 'WORD') {
    let latestWord = '';
    for (const w of words) {
      if (Object.prototype.hasOwnProperty.call(CANONICAL_WORDS, w)) {
        latestWord = CANONICAL_WORDS[w];
        if (latestWord === activeQuestion.answer) {
          return {
            type: 'MATCH',
            matchedDisplay: latestWord,
            preview: latestWord
          };
        }
      }
    }
    return {
      type: 'NO_MATCH',
      preview: latestWord
    };
  }

  return { type: 'NONE' };
}

/********************************************************************
 * 2. AUDIO DEBUG PANEL
 ********************************************************************/
const AUDIO_DEBUG_LOG_LIMIT = 100;
const audioDebugState = {
  elements: null,
  resultCount: 0,
  restartCount: 0
};

function setAudioDebugText(name, value) {
  if (!audioDebugState.elements) return;
  audioDebugState.elements[name].textContent = value;
}

function setAudioDebugStatus(label, tone = 'neutral') {
  if (!audioDebugState.elements) return;
  const toneClasses = {
    neutral: 'text-zinc-300',
    active: 'text-emerald-300',
    warning: 'text-amber-300',
    error: 'text-rose-300'
  };
  const status = audioDebugState.elements.status;
  status.textContent = label;
  status.className = `mt-1 break-words ${toneClasses[tone] || toneClasses.neutral}`;
}

function setAudioDebugPanelOpen(isOpen) {
  if (!audioDebugState.elements) return;
  const { panel, toggleBtn } = audioDebugState.elements;
  panel.classList.toggle('hidden', !isOpen);
  toggleBtn.setAttribute('aria-expanded', String(isOpen));
  toggleBtn.classList.toggle('border-zinc-500', isOpen);
  toggleBtn.classList.toggle('text-zinc-100', isOpen);
}

function clearAudioDebugLog() {
  if (!audioDebugState.elements) return;
  const log = audioDebugState.elements.log;
  log.replaceChildren();
  const empty = document.createElement('p');
  empty.dataset.audioDebugEmpty = '';
  empty.className = 'text-zinc-600';
  empty.textContent = 'No audio events yet.';
  log.appendChild(empty);
}

function logAudioDebug(eventName, detail = '', level = 'info') {
  if (!audioDebugState.elements) return;
  const { log } = audioDebugState.elements;
  setAudioDebugText('lastEvent', eventName);

  const placeholder = log.querySelector('[data-audio-debug-empty]');
  if (placeholder) placeholder.remove();

  const now = new Date();
  const pad = (value, length = 2) => String(value).padStart(length, '0');
  const timestamp = `${pad(now.getHours())}:${pad(now.getMinutes())}:${pad(now.getSeconds())}.${pad(now.getMilliseconds(), 3)}`;

  const row = document.createElement('div');
  row.className = level === 'error'
    ? 'text-rose-300'
    : (level === 'warning' ? 'text-amber-300' : 'text-zinc-300');

  const heading = document.createElement('span');
  heading.className = 'text-zinc-500';
  heading.textContent = `${timestamp}  ${eventName}`;
  row.appendChild(heading);

  if (detail) {
    const detailNode = document.createElement('span');
    detailNode.textContent = `  ${detail}`;
    row.appendChild(detailNode);
  }

  log.appendChild(row);
  while (log.children.length > AUDIO_DEBUG_LOG_LIMIT) {
    log.firstElementChild.remove();
  }
  log.scrollTop = log.scrollHeight;
}

function updateAudioDebugAlternatives(resultItem) {
  if (!audioDebugState.elements) return;
  const list = audioDebugState.elements.alternatives;
  list.replaceChildren();

  for (let i = 0; i < resultItem.length; i++) {
    const alternative = resultItem[i];
    const item = document.createElement('li');
    const confidence = Number.isFinite(alternative.confidence)
      ? ` — ${(alternative.confidence * 100).toFixed(0)}% confidence`
      : '';
    item.className = 'whitespace-pre-wrap break-words';
    item.textContent = `${i + 1}. ${alternative.transcript || '(empty)'}${confidence}`;
    list.appendChild(item);
  }

  if (resultItem.length === 0) {
    const item = document.createElement('li');
    item.className = 'text-zinc-600';
    item.textContent = 'No alternatives returned.';
    list.appendChild(item);
  }
}

function formatAudioDebugOutcome(outcome) {
  let formatted = outcome.type;
  if (outcome.matchedDisplay !== undefined) {
    formatted += ` · matched "${outcome.matchedDisplay}"`;
  } else if (outcome.preview) {
    formatted += ` · preview "${outcome.preview}"`;
  }
  return formatted;
}

function updateAudioDebugConsumed() {
  if (!audioDebugState.elements) return;
  setAudioDebugText('consumed', `${state.consumedResultIndex} / ${state.consumedTranscriptLength}`);
}

function initAudioDebugPanel() {
  const ids = {
    panel: 'audioDebugPanel',
    toggleBtn: 'audioDebugToggleBtn',
    closeBtn: 'audioDebugCloseBtn',
    clearBtn: 'audioDebugClearBtn',
    support: 'audioDebugSupport',
    status: 'audioDebugStatus',
    resultCount: 'audioDebugResultCount',
    restartCount: 'audioDebugRestartCount',
    error: 'audioDebugError',
    raw: 'audioDebugRaw',
    unconsumed: 'audioDebugUnconsumed',
    evaluated: 'audioDebugEvaluated',
    outcome: 'audioDebugOutcome',
    consumed: 'audioDebugConsumed',
    alternatives: 'audioDebugAlternatives',
    lastEvent: 'audioDebugLastEvent',
    log: 'audioDebugLog'
  };

  const elements = {};
  for (const [name, id] of Object.entries(ids)) {
    elements[name] = document.getElementById(id);
  }
  audioDebugState.elements = elements;

  elements.toggleBtn.addEventListener('click', () => {
    setAudioDebugPanelOpen(elements.panel.classList.contains('hidden'));
  });
  elements.closeBtn.addEventListener('click', () => setAudioDebugPanelOpen(false));
  elements.clearBtn.addEventListener('click', clearAudioDebugLog);

  const supported = Boolean(window.SpeechRecognition || window.webkitSpeechRecognition);
  setAudioDebugText('support', supported ? 'Available' : 'Unavailable');
  setAudioDebugText('error', 'None');
  setAudioDebugStatus('Idle');
  logAudioDebug('init', supported ? 'Web Speech API detected' : 'Web Speech API not detected', supported ? 'info' : 'error');
}

/********************************************************************
 * 3. WEB SPEECH API ENGINE (ZERO-WAIT INTERIM EVALUATION)
 ********************************************************************/
function initSpeechRecognition() {
  const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
  if (!SpeechRecognition) {
    micBtnLabel.textContent = 'Voice Unsupported';
    micToggleBtn.disabled = true;
    liveTranscript.textContent = 'Web Speech API unavailable in this browser. Use keyboard input below.';
    setAudioDebugText('support', 'Unavailable');
    setAudioDebugStatus('Unsupported', 'error');
    setAudioDebugText('error', 'SpeechRecognition is not available in this browser');
    logAudioDebug('unsupported', 'Use a browser with Web Speech API support', 'error');
    return;
  }

  const recognition = new SpeechRecognition();
  // Keep one session alive; consumed transcript offsets prevent old audio matching twice.
  recognition.continuous = true;
  recognition.interimResults = true;
  recognition.lang = 'en-US';
  recognition.maxAlternatives = 3;
  logAudioDebug('configure', 'continuous=true interimResults=true lang=en-US maxAlternatives=3');

  recognition.onstart = () => {
    state.micListening = true;
    micDot.className = 'w-2 h-2 rounded-full bg-emerald-400 mic-active-dot';
    micBtnLabel.textContent = 'Listening';
    micToggleBtn.classList.replace('border-zinc-800', 'border-emerald-500/50');
    liveTranscript.textContent = 'Listening for answer...';
    setAudioDebugStatus('Listening', 'active');
    setAudioDebugText('error', 'None');
    logAudioDebug('start', 'Recognition session started');
  };

  recognition.onaudiostart = () => logAudioDebug('audiostart', 'Microphone audio capture started');
  recognition.onsoundstart = () => logAudioDebug('soundstart', 'Sound detected');
  recognition.onspeechstart = () => logAudioDebug('speechstart', 'Speech detected');
  recognition.onspeechend = () => logAudioDebug('speechend', 'Speech ended');
  recognition.onsoundend = () => logAudioDebug('soundend', 'Sound ended');
  recognition.onaudioend = () => logAudioDebug('audioend', 'Microphone audio capture ended');
  recognition.onnomatch = () => {
    setAudioDebugText('outcome', 'NOMATCH · Speech API returned no match');
    logAudioDebug('nomatch', 'Speech was detected but no recognition match was returned', 'warning');
  };

  recognition.onresult = (event) => {
    const latestIdx = event.results.length - 1;
    const resultItem = event.results[latestIdx];
    const latestRawTranscript = resultItem[0].transcript;
    const firstResultIdx = Number.isInteger(event.resultIndex) ? event.resultIndex : latestIdx;
    const resultEntries = [];

    // Process every result changed by this event, not only the latest result block.
    for (let resultIdx = firstResultIdx; resultIdx < event.results.length; resultIdx++) {
      const item = event.results[resultIdx];
      resultEntries.push({
        index: resultIdx,
        item,
        rawTranscript: item[0].transcript
      });
    }
    const rawTranscript = resultEntries.map(entry => entry.rawTranscript).join(' ');

    audioDebugState.resultCount += 1;
    setAudioDebugText('resultCount', audioDebugState.resultCount);
    setAudioDebugText('raw', rawTranscript || '(empty)');
    setAudioDebugText('unconsumed', '—');
    setAudioDebugText('evaluated', '—');
    setAudioDebugText('outcome', 'Evaluating…');
    updateAudioDebugAlternatives(resultItem);
    updateAudioDebugConsumed();
    logAudioDebug('result', `first=${firstResultIdx} latest=${latestIdx} final=${Boolean(resultItem.isFinal)} · "${rawTranscript}"`);

    const now = performance.now();
    // 350ms cooldown after a match so residual syllables don't trigger on the new question
    if (now - state.lastMatchTimestamp < 350 || state.isTransitioning) {
      const cooldownRemaining = Math.max(0, Math.ceil(350 - (now - state.lastMatchTimestamp)));
      state.consumedResultIndex = latestIdx;
      state.consumedTranscriptLength = latestRawTranscript.length;
      setAudioDebugText('outcome', `IGNORED · ${state.isTransitioning ? 'question transition' : 'match cooldown'}`);
      updateAudioDebugConsumed();
      logAudioDebug('ignored', `transition=${state.isTransitioning} cooldown=${cooldownRemaining}ms · result consumed without parsing`, 'warning');
      return;
    }

    // Slice out text already consumed from any result block still in this session.
    if (!resultEntries.some(entry => entry.index === state.consumedResultIndex)) {
      state.consumedResultIndex = -1;
      state.consumedTranscriptLength = 0;
    }
    for (const entry of resultEntries) {
      entry.unconsumed = entry.index === state.consumedResultIndex
        ? entry.rawTranscript.slice(state.consumedTranscriptLength)
        : entry.rawTranscript;
    }
    const unconsumed = resultEntries.map(entry => entry.unconsumed).join(' ');

    setAudioDebugText('unconsumed', unconsumed || '(empty)');
    updateAudioDebugConsumed();

    const displayStr = unconsumed.trim() || rawTranscript.trim();
    if (displayStr) {
      liveTranscript.textContent = `"${displayStr}"`;
    }

    // Evaluate every changed result and all alternatives for homophone resilience.
    for (const entry of resultEntries) {
      for (let altIdx = 0; altIdx < entry.item.length; altIdx++) {
        const altRaw = entry.item[altIdx].transcript;
        const altUnconsumed = (entry.index === state.consumedResultIndex)
          ? altRaw.slice(state.consumedTranscriptLength)
          : altRaw;

        // Only evaluate the trailing window of words so old mistakes don't block new words
        const trailingWords = altUnconsumed.trim().split(/\s+/).slice(-4).join(' ');
        const outcome = evaluateUtterance(trailingWords, state.currentQuestion);
        const isSuccessfulOutcome = outcome.type === 'MATCH' || outcome.type === 'COMMAND_SKIP' || outcome.type === 'COMMAND_REVEAL';
        const isLatestPrimaryAlternative = entry.index === latestIdx && altIdx === 0;

        if (isLatestPrimaryAlternative || isSuccessfulOutcome) {
          setAudioDebugText('evaluated', `${trailingWords || '(empty)'}  [result ${entry.index}, alternative ${altIdx + 1}]`);
          setAudioDebugText('outcome', formatAudioDebugOutcome(outcome));
        }
        logAudioDebug('parse', `result=${entry.index} alt=${altIdx + 1} · "${trailingWords || '(empty)'}" → ${formatAudioDebugOutcome(outcome)}`);

        if (isLatestPrimaryAlternative) {
          if (outcome.preview) {
            parsedPreview.textContent = `→ ${outcome.preview}`;
            parsedPreview.classList.remove('hidden');
          } else {
            parsedPreview.classList.add('hidden');
          }
        }

        if (outcome.type === 'COMMAND_SKIP') {
          state.consumedResultIndex = latestIdx;
          state.consumedTranscriptLength = latestRawTranscript.length;
          state.lastMatchTimestamp = performance.now();
          updateAudioDebugConsumed();
          logAudioDebug('command', `SKIP accepted from result ${entry.index}, alternative ${altIdx + 1}`);
          nextQuestion();
          return;
        }

        if (outcome.type === 'COMMAND_REVEAL') {
          state.consumedResultIndex = latestIdx;
          state.consumedTranscriptLength = latestRawTranscript.length;
          updateAudioDebugConsumed();
          logAudioDebug('command', `REVEAL accepted from result ${entry.index}, alternative ${altIdx + 1}`);
          triggerRevealTransition();
          return;
        }

        if (outcome.type === 'MATCH') {
          state.consumedResultIndex = latestIdx;
          state.consumedTranscriptLength = latestRawTranscript.length;
          updateAudioDebugConsumed();
          logAudioDebug('match', `"${outcome.matchedDisplay}" accepted from result ${entry.index}, alternative ${altIdx + 1}`);
          triggerCorrectTransition(outcome.matchedDisplay);
          return;
        }
      }
    }
  };

  recognition.onerror = (event) => {
    const errorDetail = event.message ? `${event.error}: ${event.message}` : event.error;
    setAudioDebugText('error', errorDetail || 'Unknown recognition error');
    logAudioDebug('error', errorDetail || 'Unknown recognition error', 'error');

    if (event.error === 'not-allowed' || event.error === 'service-not-allowed') {
      state.micListening = false;
      micDot.className = 'w-2 h-2 rounded-full bg-rose-500';
      micBtnLabel.textContent = 'Mic Blocked';
      liveTranscript.textContent = 'Microphone permission denied. Type answers below.';
      setAudioDebugStatus('Permission Blocked', 'error');
    } else {
      setAudioDebugStatus(`Error: ${event.error}`, 'error');
    }
  };

  recognition.onend = () => {
    logAudioDebug('end', `Recognition ended · micListening=${state.micListening}`);
    // Auto-restart continuous session if user did not manually pause
    if (state.micListening) {
      try {
        state.consumedResultIndex = -1;
        state.consumedTranscriptLength = 0;
        audioDebugState.restartCount += 1;
        setAudioDebugText('restartCount', audioDebugState.restartCount);
        setAudioDebugStatus('Restarting…', 'warning');
        updateAudioDebugConsumed();
        logAudioDebug('restart', `Attempt ${audioDebugState.restartCount}`, 'warning');
        recognition.start();
      } catch (e) {
        // Ignore race condition on rapid restart
        setAudioDebugText('error', `Restart failed: ${e.message}`);
        setAudioDebugStatus('Restart Failed', 'error');
        logAudioDebug('restart-error', e.message, 'error');
      }
    } else {
      micDot.className = 'w-2 h-2 rounded-full bg-zinc-500';
      micBtnLabel.textContent = 'Start Mic';
      micToggleBtn.classList.replace('border-emerald-500/50', 'border-zinc-800');
      setAudioDebugStatus('Paused');
    }
  };

  state.recognition = recognition;
}

function toggleMicrophone() {
  if (!state.recognition) {
    logAudioDebug('mic-toggle', 'No recognition instance; initializing');
    initSpeechRecognition();
  }
  if (!state.recognition) return;

  if (state.micListening) {
    state.micListening = false;
    setAudioDebugStatus('Stopping…', 'warning');
    logAudioDebug('mic-toggle', 'Stop requested');
    state.recognition.stop();
    liveTranscript.textContent = 'Microphone paused.';
    parsedPreview.classList.add('hidden');
  } else {
    try {
      state.consumedResultIndex = -1;
      state.consumedTranscriptLength = 0;
      setAudioDebugStatus('Starting…', 'warning');
      updateAudioDebugConsumed();
      logAudioDebug('mic-toggle', 'Start requested');
      state.recognition.start();
    } catch (err) {
      // Already started
      setAudioDebugText('error', `Start failed: ${err.message}`);
      setAudioDebugStatus('Start Failed', 'error');
      logAudioDebug('start-error', err.message, 'error');
    }
  }
}
