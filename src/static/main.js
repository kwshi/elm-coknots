const app = Elm.Main.init({
  node: document.body,
});

const gaussInput = document.getElementById("gauss");
const updateSelection = ({ target }) => {
  app.ports.selection.send([target.selectionStart, target.selectionEnd]);
};
const sanitizeGauss = (s) => s.toLowerCase().replace(/[^\d uo+-]/, "");

const setGauss = (s) => {
  gaussInput.value = s;
  app.ports.input.send([null, gaussInput.value]);
};
app.ports.setInput.subscribe(setGauss);

const setSelection = ([i, j]) => {
  gaussInput.selectionStart = i;
  gaussInput.selectionEnd = j;
};
app.ports.setSelection.subscribe(setSelection);

gaussInput.addEventListener("input", ({ target }) => {
  const i = target.selectionStart;
  const pre = sanitizeGauss(target.value.slice(0, i));
  const suf = sanitizeGauss(target.value.slice(i));
  target.value = pre + suf;
  target.selectionStart = target.selectionEnd = pre.length;
  app.ports.input.send([target.selectionStart, target.value]);
});
gaussInput.addEventListener("focus", updateSelection);
gaussInput.addEventListener("click", updateSelection);
gaussInput.addEventListener("keyup", updateSelection);
gaussInput.addEventListener("touchstart", updateSelection);
gaussInput.addEventListener("touchend", updateSelection);
