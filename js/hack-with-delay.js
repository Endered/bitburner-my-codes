function sleep(ms){
  return new Promise(resolve => setTimeout(resolve, ms));
}

/** @param {NS} ns */
export async function main(ns) {
  await sleep(Number(ns.args[1]))
  await ns.hack(ns.args[0]);
}
