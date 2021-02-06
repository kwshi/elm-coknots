const app = Elm.Main.init({
  node: document.body,
});

const gaussInput = document.getElementById("gauss");
const updateSelection = ({ target }) => {
  app.ports.selection.send([target.selectionStart, target.selectionEnd]);
};
const sanitizeGauss = (s) => s.toLowerCase().replace(/[^\d uo+-]/, "");

app.ports.setInput.subscribe((e) => {
  gaussInput.value = e;
  console.log(e);
  app.ports.input.send([gaussInput.selectionStart, gaussInput.value]);
});

gaussInput.addEventListener("input", ({ target }) => {
  const i = target.selectionStart;
  const pre = sanitizeGauss(target.value.slice(0, i));
  const suf = sanitizeGauss(target.value.slice(i));
  target.value = pre + suf;
  target.selectionStart = target.selectionEnd = pre.length;
  app.ports.input.send([target.selectionStart, target.value]);
});
gaussInput.addEventListener("focus", updateSelection);
gaussInput.addEventListener("selectionchange", updateSelection);
gaussInput.addEventListener("click", updateSelection);
gaussInput.addEventListener("keydown", updateSelection);
gaussInput.addEventListener("touchstart", updateSelection);
