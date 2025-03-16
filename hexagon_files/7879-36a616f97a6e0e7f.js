(self.webpackChunk_N_E=self.webpackChunk_N_E||[]).push([[7879],{44713:function(e,t,n){var r=0/0,i=/^\s+|\s+$/g,o=/^[-+]0x[0-9a-f]+$/i,u=/^0b[01]+$/i,a=/^0o[0-7]+$/i,c=parseInt,s="object"==typeof n.g&&n.g&&n.g.Object===Object&&n.g,l="object"==typeof self&&self&&self.Object===Object&&self,f=s||l||Function("return this")(),d=Object.prototype.toString,g=Math.max,v=Math.min,w=function(){return f.Date.now()};function h(e){var t=typeof e;return!!e&&("object"==t||"function"==t)}function y(e){if("number"==typeof e)return e;if("symbol"==typeof(t=e)||t&&"object"==typeof t&&"[object Symbol]"==d.call(t))return r;if(h(e)){var t,n="function"==typeof e.valueOf?e.valueOf():e;e=h(n)?n+"":n}if("string"!=typeof e)return 0===e?e:+e;e=e.replace(i,"");var s=u.test(e);return s||a.test(e)?c(e.slice(2),s?2:8):o.test(e)?r:+e}e.exports=function(e,t,n){var r,i,o,u,a,c,s=0,l=!1,f=!1,d=!0;if("function"!=typeof e)throw TypeError("Expected a function");function b(t){var n=r,o=i;return r=i=void 0,s=t,u=e.apply(o,n)}function p(e){var n=e-c,r=e-s;return void 0===c||n>=t||n<0||f&&r>=o}function S(){var e,n,r,i=w();if(p(i))return m(i);a=setTimeout(S,(e=i-c,n=i-s,r=t-e,f?v(r,o-n):r))}function m(e){return(a=void 0,d&&r)?b(e):(r=i=void 0,u)}function E(){var e,n=w(),o=p(n);if(r=arguments,i=this,c=n,o){if(void 0===a)return s=e=c,a=setTimeout(S,t),l?b(e):u;if(f)return a=setTimeout(S,t),b(c)}return void 0===a&&(a=setTimeout(S,t)),u}return t=y(t)||0,h(n)&&(l=!!n.leading,o=(f="maxWait"in n)?g(y(n.maxWait)||0,t):o,d="trailing"in n?!!n.trailing:d),E.cancel=function(){void 0!==a&&clearTimeout(a),s=0,r=c=i=a=void 0},E.flush=function(){return void 0===a?u:m(w())},E}},77879:function(e,t,n){"use strict";n.d(t,{Cf:function(){return s},KS:function(){return k},O_:function(){return p},S1:function(){return b},Xs:function(){return m},Yz:function(){return l},_:function(){return g},ac:function(){return w},g4:function(){return E},iP:function(){return I},kt:function(){return o},m9:function(){return c},nj:function(){return y}});var r=n(7653),i=n(44713);function o(e=!1){if("boolean"!=typeof e)throw Error("defaultValue must be `true` or `false`");let[t,n]=(0,r.useState)(e),i=(0,r.useCallback)(()=>{n(!0)},[]),o=(0,r.useCallback)(()=>{n(!1)},[]),u=(0,r.useCallback)(()=>{n(e=>!e)},[]);return{value:t,setValue:n,setTrue:i,setFalse:o,toggle:u}}var u="undefined"!=typeof window?r.useLayoutEffect:r.useEffect;function a(e,t,n,i){let o=(0,r.useRef)(t);u(()=>{o.current=t},[t]),(0,r.useEffect)(()=>{let t=(null==n?void 0:n.current)??window;if(!(t&&t.addEventListener))return;let r=e=>{o.current(e)};return t.addEventListener(e,r,i),()=>{t.removeEventListener(e,r,i)}},[e,n,i])}function c(){let[e,t]=(0,r.useState)(null);return[e,(0,r.useCallback)(async e=>{if(!(null==navigator?void 0:navigator.clipboard))return console.warn("Clipboard not supported"),!1;try{return await navigator.clipboard.writeText(e),t(e),!0}catch(e){return console.warn("Copy failed",e),t(null),!1}},[])]}function s(e){let[t,n]=(0,r.useState)(e??0);return{count:t,increment:(0,r.useCallback)(()=>{n(e=>e+1)},[]),decrement:(0,r.useCallback)(()=>{n(e=>e-1)},[]),reset:(0,r.useCallback)(()=>{n(e??0)},[e]),setCount:n}}function l(e,t){let n=(0,r.useRef)(e);u(()=>{n.current=e},[e]),(0,r.useEffect)(()=>{if(null===t)return;let e=setInterval(()=>{n.current()},t);return()=>{clearInterval(e)}},[t])}function f(e){let t=(0,r.useRef)(()=>{throw Error("Cannot call an event handler while rendering.")});return u(()=>{t.current=e},[e]),(0,r.useCallback)((...e)=>{var n;return null==(n=t.current)?void 0:n.call(t,...e)},[t])}var d="undefined"==typeof window;function g(e,t,n={}){let{initializeWithValue:i=!0}=n,o=(0,r.useCallback)(e=>n.serializer?n.serializer(e):JSON.stringify(e),[n]),u=(0,r.useCallback)(e=>{let r;if(n.deserializer)return n.deserializer(e);if("undefined"===e)return;let i=t instanceof Function?t():t;try{r=JSON.parse(e)}catch(e){return console.error("Error parsing JSON:",e),i}return r},[n,t]),c=(0,r.useCallback)(()=>{let n=t instanceof Function?t():t;if(d)return n;try{let t=window.localStorage.getItem(e);return t?u(t):n}catch(t){return console.warn(`Error reading localStorage key \u201C${e}\u201D:`,t),n}},[t,e,u]),[s,l]=(0,r.useState)(()=>i?c():t instanceof Function?t():t),g=f(t=>{d&&console.warn(`Tried setting localStorage key \u201C${e}\u201D even though environment is not a client`);try{let n=t instanceof Function?t(c()):t;window.localStorage.setItem(e,o(n)),l(n),window.dispatchEvent(new StorageEvent("local-storage",{key:e}))}catch(t){console.warn(`Error setting localStorage key \u201C${e}\u201D:`,t)}}),v=f(()=>{d&&console.warn(`Tried removing localStorage key \u201C${e}\u201D even though environment is not a client`);let n=t instanceof Function?t():t;window.localStorage.removeItem(e),l(n),window.dispatchEvent(new StorageEvent("local-storage",{key:e}))});(0,r.useEffect)(()=>{l(c())},[e]);let w=(0,r.useCallback)(t=>{t.key&&t.key!==e||l(c())},[e,c]);return a("storage",w),a("local-storage",w),[s,g,v]}var v="undefined"==typeof window;function w(e,{defaultValue:t=!1,initializeWithValue:n=!0}={}){let i=e=>v?t:window.matchMedia(e).matches,[o,a]=(0,r.useState)(()=>n?i(e):t);function c(){a(i(e))}return u(()=>{let t=window.matchMedia(e);return c(),t.addListener?t.addListener(c):t.addEventListener("change",c),()=>{t.removeListener?t.removeListener(c):t.removeEventListener("change",c)}},[e]),o}function h(e,t=500,n){let o=(0,r.useRef)();!function(e){let t=(0,r.useRef)(e);t.current=e,(0,r.useEffect)(()=>()=>{t.current()},[])}(()=>{o.current&&o.current.cancel()});let u=(0,r.useMemo)(()=>{let r=i(e,t,n),u=(...e)=>r(...e);return u.cancel=()=>{r.cancel()},u.isPending=()=>!!o.current,u.flush=()=>r.flush(),u},[e,t,n]);return(0,r.useEffect)(()=>{o.current=i(e,t,n)},[e,t,n]),u}function y(e,t,n){let i=(null==n?void 0:n.equalityFn)??((e,t)=>e===t),o=e instanceof Function?e():e,[u,a]=(0,r.useState)(o),c=(0,r.useRef)(o),s=h(a,t,n);return i(c.current,o)||(s(o),c.current=o),[u,s]}function b({threshold:e=0,root:t=null,rootMargin:n="0%",freezeOnceVisible:i=!1,initialIsIntersecting:o=!1,onChange:u}={}){var a;let[c,s]=(0,r.useState)(null),[l,f]=(0,r.useState)(()=>({isIntersecting:o,entry:void 0})),d=(0,r.useRef)();d.current=u;let g=(null==(a=l.entry)?void 0:a.isIntersecting)&&i;(0,r.useEffect)(()=>{let r;if(!c||!("IntersectionObserver"in window)||g)return;let o=new IntersectionObserver(e=>{let t=Array.isArray(o.thresholds)?o.thresholds:[o.thresholds];e.forEach(e=>{let n=e.isIntersecting&&t.some(t=>e.intersectionRatio>=t);f({isIntersecting:n,entry:e}),d.current&&d.current(n,e),n&&i&&r&&(r(),r=void 0)})},{threshold:e,root:t,rootMargin:n});return o.observe(c),()=>{o.disconnect()}},[c,JSON.stringify(e),t,n,g,i]);let v=(0,r.useRef)(null);(0,r.useEffect)(()=>{var e;c||null==(e=l.entry)||!e.target||i||g||v.current===l.entry.target||(v.current=l.entry.target,f({isIntersecting:o,entry:void 0}))},[c,l.entry,i,g,o]);let w=[s,!!l.isIntersecting,l.entry];return w.ref=w[0],w.isIntersecting=w[1],w.entry=w[2],w}function p(){let[e,t]=(0,r.useState)(!1);return(0,r.useEffect)(()=>{t(!0)},[]),e}var S="undefined"==typeof window;function m(e,t,n={}){let{initializeWithValue:i=!0}=n,o=(0,r.useCallback)(e=>n.serializer?n.serializer(e):JSON.stringify(e),[n]),u=(0,r.useCallback)(e=>{let r;if(n.deserializer)return n.deserializer(e);if("undefined"===e)return;let i=t instanceof Function?t():t;try{r=JSON.parse(e)}catch(e){return console.error("Error parsing JSON:",e),i}return r},[n,t]),c=(0,r.useCallback)(()=>{let n=t instanceof Function?t():t;if(S)return n;try{let t=window.sessionStorage.getItem(e);return t?u(t):n}catch(t){return console.warn(`Error reading sessionStorage key \u201C${e}\u201D:`,t),n}},[t,e,u]),[s,l]=(0,r.useState)(()=>i?c():t instanceof Function?t():t),d=f(t=>{S&&console.warn(`Tried setting sessionStorage key \u201C${e}\u201D even though environment is not a client`);try{let n=t instanceof Function?t(c()):t;window.sessionStorage.setItem(e,o(n)),l(n),window.dispatchEvent(new StorageEvent("session-storage",{key:e}))}catch(t){console.warn(`Error setting sessionStorage key \u201C${e}\u201D:`,t)}}),g=f(()=>{S&&console.warn(`Tried removing sessionStorage key \u201C${e}\u201D even though environment is not a client`);let n=t instanceof Function?t():t;window.sessionStorage.removeItem(e),l(n),window.dispatchEvent(new StorageEvent("session-storage",{key:e}))});(0,r.useEffect)(()=>{l(c())},[e]);let v=(0,r.useCallback)(t=>{t.key&&t.key!==e||l(c())},[e,c]);return a("storage",v),a("session-storage",v),[s,d,g]}function E(e){let[t,n]=(0,r.useState)(1),i=t+1<=e,o=t-1>0,u=(0,r.useCallback)(r=>{let i=r instanceof Function?r(t):r;if(i>=1&&i<=e){n(i);return}throw Error("Step not valid")},[e,t]);return[t,{goToNextStep:(0,r.useCallback)(()=>{i&&n(e=>e+1)},[i]),goToPrevStep:(0,r.useCallback)(()=>{o&&n(e=>e-1)},[o]),canGoToNextStep:i,canGoToPrevStep:o,setStep:u,reset:(0,r.useCallback)(()=>{n(1)},[])}]}function k(e,t){let n=(0,r.useRef)(e);u(()=>{n.current=e},[e]),(0,r.useEffect)(()=>{if(!t&&0!==t)return;let e=setTimeout(()=>{n.current()},t);return()=>{clearTimeout(e)}},[t])}var C="undefined"==typeof window;function I(e={}){let{initializeWithValue:t=!0}=e;C&&(t=!1);let[n,i]=(0,r.useState)(()=>t?{width:window.innerWidth,height:window.innerHeight}:{width:void 0,height:void 0}),o=h(i,e.debounceDelay);function c(){(e.debounceDelay?o:i)({width:window.innerWidth,height:window.innerHeight})}return a("resize",c),u(()=>{c()},[]),n}}}]);