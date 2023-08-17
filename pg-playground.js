const pg = require("pg");

const connstr = process.env.PG_CONNSTR;
if (!connstr) throw new Error("env var missing `PG_CONNSTR`");

const client = new pg.Client(connstr);
client.connect((err) => {
  if (err) throw new Error(err);

  console.log("[INFO] `pg.Client` connected ✅");
});

function go(query, params = []) {
  client.query(query, params, (err, res) => {
    console.log(res);
    if (err) console.log("ERR", err);
    else console.log(JSON.stringify(res.rows));
  });
}

console.log("pg-playground.js");
console.log("usage: `go(query, params)`");
