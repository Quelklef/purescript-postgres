import pg from 'pg';

// See Query.js
// Not sure why it needs to be repeated here...
pg.types.getTypeParser = () => x => x;

export const open_f =
connectionString =>
async function()
{
  const client = new pg.Client({ connectionString });
  await client.connect();
  return client;
};
