import path from "node:path";

export const normalizeDriveLetter = (p) => {
  const parts = path.parse(p)
  return path.format({
    ...parts,
    dir: (parts.dir.charAt(0) ?? "").toLowerCase() + parts.dir.slice(1)
  })
}
