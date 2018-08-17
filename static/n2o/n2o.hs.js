function uint64(i) { var buf = new ArrayBuffer(8); new DataView(buf).setBigUint64(0, BigInt(i)); return new Uint8Array(buf); }
function int64(i) { var buf = new ArrayBuffer(8); new DataView(buf).setBigInt64(0, BigInt(i)); return new Uint8Array(buf); }
function utf8_text(t) { return new TextEncoder('utf-8').encode(t); }
function merge(a, b) { var c = new Uint8Array(a.length + b.length); c.set(a); c.set(b, a.length); return c; }
function mergeAll(list) { return list.reduce(merge); }
function text(t) { var a1 = utf8_text(t), a2 = uint64(a1.length); return merge(a2, a1); }
function float(n) { }
function getNumberParts(x) {
    if(isNaN(x)) {
        return {mantissa: -6755399441055744, exponent: 972};
    }
    var sig = x > 0 ? 1 : -1;
    if(!isFinite(x)) {
        return {mantissa: sig * 4503599627370496, exponent: 972};
    }
    x = Math.abs(x);
    var exp = Math.floor(Math.log(x)*Math.LOG2E)-52;
    var man = x/Math.pow(2, exp);
    return {mantissa: sig*man, exponent: exp};
}
function data(d, args) {
    var i = transition.events.indexOf(d), buf = new ArrayBuffer(1);
    new DataView(buf).setUint8(0, i);
    return [new Uint8Array(buf)].concat(args).reduce(merge);
}
function querySource2(id) { return text(querySourceRaw(id)); }
