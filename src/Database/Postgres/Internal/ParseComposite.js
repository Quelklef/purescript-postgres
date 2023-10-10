export const parseComposite_f =
({ left, right, just, nothing }) =>
{

  // https://www.postgresql.org/docs/9.0/arrays.html#ARRAYS-IO
  // https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-IO-SYNTAX

  return ({ open, delim, close, compositeType }) => expr => {
    if (compositeType === "array" && expr === `${open}${close}`)
      return right([]);

    const specials = new Set([open, delim, close]);

    let i = 0;

    if (expr[i] === open)
      i += open.length;
    else
      return expected(expr, open, i);

    const subexprs = [];

    while (true) {
      let subexpr;
      [subexpr, i] = readSubexpr(expr, i, specials, compositeType);
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

  function readSubexpr(expr, i, specials, compositeType) {
    const isQuoted = expr[i] === '"';
    let subexpr = "";

    if (isQuoted) {
      i++;

      if (compositeType === "tuple") {
        while (
          i < expr.length - 1 &&
          !(expr[i] === '"' && expr[i + 1] !== '"')
        ) {
          subexpr += expr[i];
          if (expr[i] === '"' && expr[i + 1] === '"') i += 2;
          else i++;
        }
      } else if (compositeType === "array") {
        while (i < expr.length && expr[i] !== '"') {
          if (expr[i] === "\\") i++;
          subexpr += expr[i];
          i++;
        }
      } else {
        throw new Error(
          '`isQuoted` but `compositeType` is not `"tuple"` or `"array"`'
        );
      }

      i++;
    } else {
      while (i < expr.length && !specials.has(expr[i])) {
        subexpr += expr[i];
        i++;
      }
    }

    if (isQuoted) return [just(subexpr), i];
    if (compositeType === "array" && subexpr === "NULL") return [nothing, i];
    if (compositeType === "tuple" && subexpr === "") return [nothing, i];
    return [just(subexpr), i];
  }
}
