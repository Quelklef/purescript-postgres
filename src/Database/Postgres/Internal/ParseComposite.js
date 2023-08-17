export const parseComposite_f =
({ left, right, just, nothing }) =>
{

  // https://www.postgresql.org/docs/9.0/arrays.html#ARRAYS-IO
  // https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-IO-SYNTAX

  return ({ open, delim, close, exprIsNull }) => expr => {
    const specials = new Set([open, delim, close]);

    let i = 0;

    if (expr[i] === open)
      i += open.length;
    else
      return expected(expr, open, i);

    const subexprs = [];

    while (true) {
      if (i >= expr.length - 1) break;

      let subexpr;
      [subexpr, i] = readSubexpr(expr, i, specials, exprIsNull);
      subexprs.push(subexpr);

      if (i >= expr.length - 1) break;

      if (expr[i] === delim)
        i += delim.length;
      else
        return expected(expr, delim, i);
    }

    if (expr[i] === close)
      i += close.length;
    else
      return expected(expr, close, i);

    return right(subexprs);
  }

  function expected(expr, what, at) {
    return left(
        `Expected '${what}' at index ${at} but got '${expr[at]}'\n`
      + `    ${expr}\n`
      + `    ${' '.repeat(at)}^`
    );
  }

  function readSubexpr(expr, i, specials, exprIsNull) {
    const isQuoted = expr[i] === '"';

    if (isQuoted) {
      specials = new Set(['"', '\\']);
      i++;
    }

    let subexpr = '';

    let j = i;
    while (
      j < expr.length
      && !specials.has(expr[j])
    ) {
      j += expr[j] === '\\';
      subexpr += expr[j];
      j++;
    }

    if (isQuoted) j++;

    const result = exprIsNull(subexpr) ? nothing : just(subexpr);
    return [result, j];
  }
}
