import tailwindcss from "@tailwindcss/vite";
import elmPlugin from "vite-plugin-elm";

export default {
	plugins: [elmPlugin(), tailwindcss()],
};
