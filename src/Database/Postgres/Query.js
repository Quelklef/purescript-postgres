import pg from 'pg';

// Suppress node-postgres parsing values in retrieved rows
// This relies on implementation details of node-postgres
pg.types.getTypeParser = () => x => x;

// As it turns out, formatting of inserted rows doesn't also need to be disabled
// We can just pass PostgreSQL expressions as strings and it'll work

export const query_f =
({ conn, sql, params }) =>
async function()
{
  let query = { text: sql, rowMode: 'array' };

  if (params.length)
    query.values = params;

  const returned = await conn.query(query);

  let rows;

  // If the SQL had only one statement, then the result will be an object.
  // If it had multiple, the result will be an array of objects.
  // In the case of multiple statements, pretend like no rows were returned.
  rows = Array.isArray(returned) ? [] : returned.rows;

  return rows;
};
