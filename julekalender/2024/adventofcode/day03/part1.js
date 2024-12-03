const fs = require("fs");

input = fs.readFileSync("./example2.txt").toString();
const regex = /mul\((\d*),(\d*)\)/g;
const matches = Array.from(
  input.matchAll(regex),
  (m) => parseInt(m[1]) * parseInt(m[2]),
);
const sum = matches.reduce((acc, i) => acc + i);
console.log(sum);
