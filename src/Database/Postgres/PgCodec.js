export const replace_f = (mapping) => (string) => {
  const keys = Object.keys(mapping).sort((k1, k2) => k2.length - k1.length);

  let result = "";

  let i = 0;
  while (i < string.length) {
    let found = false;
    for (const key of keys) {
      if (string.startsWith(key, i)) {
        result += mapping[key];
        i += key.length;
        found = true;
        break;
      }
    }
    if (!found) {
      result += string[i];
      i++;
    }
  }

  return result;
};
