function sleep(ms){
  return new Promise(resolve => setTimeout(resolve, ms));
}

/** @param {NS} ns */
export async function main(ns) {
  await sleep(parseInt(ns.args[1]))
  await ns.weaken(ns.args[0]);
}
