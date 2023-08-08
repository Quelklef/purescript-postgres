export const parseComposite_f =
({ left, right, just, nothing }) =>
{

  // https://www.postgresql.org/docs/9.0/arrays.html#ARRAYS-IO
  // https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-IO-SYNTAX

  return ({ open, delim, close }) => expr => {
    const specials = new Set([open, delim, close]);

    let i = 0;

    if (expr[i] === open)
      i += open.length;
    else
      return expected(open, i);

    const subexprs = [];

    while (true) {
      if (i >= expr.length - 1) break;

      let subexpr;
      [subexpr, i] = readSubexpr(expr, i, specials);
      subexprs.push(subexpr);

      if (i >= expr.length - 1) break;

      if (expr[i] === delim)
        i += delim.length;
      else
        return expected(delim, i);
    }

    if (expr[i] === close)
      i += close.length;
    else
      return expected(close, i);

    return right(subexprs);
  }

  function expected(what, at) {
    return left(
        `Expected '${what}' at index ${at} but got '${expr[at]}'\n`
      + `    ${expr}\n`
      + `    ${' '.repeat(at)}^`
    );
  }

  function readSubexpr(expr, i, specials) {
    if (specials.has(expr[i])) {
      // subexpr is empty string, which represents null
      // return `nothing` to indicate null
      return nothing;
    }

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

    return [just(subexpr), j]
  }

}
