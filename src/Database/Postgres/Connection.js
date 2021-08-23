const pg = require('pg');

exports.open_f =
connectionString =>
async function()
{
  const client = new pg.Client({ connectionString });
  await client.connect();
  return client;
};
