// extract-selectors.js
const listSelectors = require("list-selectors");
const fs = require("fs");

listSelectors(
  ["./.main-tmp.css"],
  { include: ["classes"] },
  ({ classes }) => {
    fs.writeFileSync(
      "./css-classes.json",
      JSON.stringify(
        classes.map(c =>
          c
            .substring(1)
            .split("\\")
            .join("")
        )
      )
    );
  }
)