const fs = require("fs");

input = fs.readFileSync("./input.txt").toString();
// s-flag means "single line", which enables dotall
// dotall makes . match all characters, including newlines
const disabled = /don't\(\).*?do\(\)/gs;
const operations = /mul\((\d*),(\d*)\)/g;
const matches = Array.from(
  input.replace(disabled, "").matchAll(operations),
  (m) => parseInt(m[1]) * parseInt(m[2]),
);
const sum = matches.reduce((acc, i) => acc + i);
console.log(sum);
