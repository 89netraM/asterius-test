import * as rts from "./rts.mjs";
import module from "./calling-hs.wasm.mjs";
import req from "./calling-hs.req.mjs";

(async () => {
	req.module = await module;
	const i = await rts.newAsteriusInstance(req);
	console.log("Exported functions. Right-click and store this object:", i.exports);
})();
