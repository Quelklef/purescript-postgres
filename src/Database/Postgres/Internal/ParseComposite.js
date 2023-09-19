export const parseComposite_f =
({ left, right, just, nothing }) =>
{

  // https://www.postgresql.org/docs/9.0/arrays.html#ARRAYS-IO
  // https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-IO-SYNTAX

  return ({ open, delim, close, exprIsNull, escapeStyle }) => expr => {
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
      [subexpr, i] = readSubexpr(expr, i, specials, exprIsNull, escapeStyle);
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

  function readSubexpr(expr, i, specials, exprIsNull, escapeStyle) {
    const isQuoted = expr[i] === '"';
    let subexpr = "";

    if (isQuoted) {
      i++;

      if (escapeStyle === "double") {
        while (
          i < expr.length - 1 &&
          !(expr[i] === '"' && expr[i + 1] !== '"')
        ) {
          subexpr += expr[i];
          if (expr[i] === '"' && expr[i + 1] === '"') i += 2;
          else i++;
        }
      } else if (escapeStyle === "backslash") {
        while (i < expr.length && expr[i] !== '"') {
          if (expr[i] === "\\") i++;
          subexpr += expr[i];
          i++;
        }
      } else {
        throw new Error(
          '`isQuoted` but `escapeStyle` is not `"double"` or `"backslash"`'
        );
      }

      i++;
    } else {
      while (i < expr.length && !specials.has(expr[i])) {
        subexpr += expr[i];
        i++;
      }
    }

    const result = exprIsNull(subexpr) ? nothing : just(subexpr);
    return [result, i];
  }
}
