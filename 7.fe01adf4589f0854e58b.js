(window.webpackJsonp=window.webpackJsonp||[]).push([[7],{dAlA:function(t,e,r){(function(t){var r,o=function(t){"use strict";var e=9007199254740992,r=l(e),n="function"==typeof BigInt;function i(t,e,r,o){return void 0===t?i[0]:void 0!==e&&(10!=+e||r)?z(t,e,r,o):Y(t)}function u(t,e){this.value=t,this.sign=e,this.isSmall=!1}function p(t){this.value=t,this.sign=t<0,this.isSmall=!0}function a(t){this.value=t}function s(t){return-e<t&&t<e}function l(t){return t<1e7?[t]:t<1e14?[t%1e7,Math.floor(t/1e7)]:[t%1e7,Math.floor(t/1e7)%1e7,Math.floor(t/1e14)]}function f(t){v(t);var e=t.length;if(e<4&&O(t,r)<0)switch(e){case 0:return 0;case 1:return t[0];case 2:return t[0]+1e7*t[1];default:return t[0]+1e7*(t[1]+1e7*t[2])}return t}function v(t){for(var e=t.length;0===t[--e];);t.length=e+1}function h(t){for(var e=new Array(t),r=-1;++r<t;)e[r]=0;return e}function y(t){return t>0?Math.floor(t):Math.ceil(t)}function c(t,e){var r,o,n=t.length,i=e.length,u=new Array(n),p=0;for(o=0;o<i;o++)p=(r=t[o]+e[o]+p)>=1e7?1:0,u[o]=r-1e7*p;for(;o<n;)p=1e7===(r=t[o]+p)?1:0,u[o++]=r-1e7*p;return p>0&&u.push(p),u}function g(t,e){return t.length>=e.length?c(t,e):c(e,t)}function m(t,e){var r,o,n=t.length,i=new Array(n);for(o=0;o<n;o++)r=t[o]-1e7+e,e=Math.floor(r/1e7),i[o]=r-1e7*e,e+=1;for(;e>0;)i[o++]=e%1e7,e=Math.floor(e/1e7);return i}function d(t,e){var r,o,n=t.length,i=e.length,u=new Array(n),p=0;for(r=0;r<i;r++)(o=t[r]-p-e[r])<0?(o+=1e7,p=1):p=0,u[r]=o;for(r=i;r<n;r++){if(!((o=t[r]-p)<0)){u[r++]=o;break}o+=1e7,u[r]=o}for(;r<n;r++)u[r]=t[r];return v(u),u}function w(t,e,r){var o,n,i=t.length,a=new Array(i),s=-e;for(o=0;o<i;o++)n=t[o]+s,s=Math.floor(n/1e7),n%=1e7,a[o]=n<0?n+1e7:n;return"number"==typeof(a=f(a))?(r&&(a=-a),new p(a)):new u(a,r)}function b(t,e){var r,o,n,i,u=t.length,p=e.length,a=h(u+p);for(n=0;n<u;++n){i=t[n];for(var s=0;s<p;++s)r=i*e[s]+a[n+s],o=Math.floor(r/1e7),a[n+s]=r-1e7*o,a[n+s+1]+=o}return v(a),a}function S(t,e){var r,o,n=t.length,i=new Array(n),u=0;for(o=0;o<n;o++)r=t[o]*e+u,u=Math.floor(r/1e7),i[o]=r-1e7*u;for(;u>0;)i[o++]=u%1e7,u=Math.floor(u/1e7);return i}function q(t,e){for(var r=[];e-- >0;)r.push(0);return r.concat(t)}function M(t,e,r){return new u(t<1e7?S(e,t):b(e,l(t)),r)}function N(t){var e,r,o,n,i=t.length,u=h(i+i);for(o=0;o<i;o++){r=0-(n=t[o])*n;for(var p=o;p<i;p++)e=n*t[p]*2+u[o+p]+r,r=Math.floor(e/1e7),u[o+p]=e-1e7*r;u[o+i]=r}return v(u),u}function E(t,e){var r,o,n,i,u=t.length,p=h(u);for(n=0,r=u-1;r>=0;--r)n=(i=1e7*n+t[r])-(o=y(i/e))*e,p[r]=0|o;return[p,0|n]}function I(t,e){var r,o=Y(e);if(n)return[new a(t.value/o.value),new a(t.value%o.value)];var s,c=t.value,g=o.value;if(0===g)throw new Error("Cannot divide by zero");if(t.isSmall)return o.isSmall?[new p(y(c/g)),new p(c%g)]:[i[0],t];if(o.isSmall){if(1===g)return[t,i[0]];if(-1==g)return[t.negate(),i[0]];var m=Math.abs(g);if(m<1e7){s=f((r=E(c,m))[0]);var w=r[1];return t.sign&&(w=-w),"number"==typeof s?(t.sign!==o.sign&&(s=-s),[new p(s),new p(w)]):[new u(s,t.sign!==o.sign),new p(w)]}g=l(m)}var b=O(c,g);if(-1===b)return[i[0],t];if(0===b)return[i[t.sign===o.sign?1:-1],i[0]];s=(r=c.length+g.length<=200?function(t,e){var r,o,n,i,u,p,a,s=t.length,l=e.length,v=h(e.length),y=e[l-1],c=Math.ceil(1e7/(2*y)),g=S(t,c),m=S(e,c);for(g.length<=s&&g.push(0),m.push(0),y=m[l-1],o=s-l;o>=0;o--){for(r=1e7-1,g[o+l]!==y&&(r=Math.floor((1e7*g[o+l]+g[o+l-1])/y)),n=0,i=0,p=m.length,u=0;u<p;u++)n+=r*m[u],a=Math.floor(n/1e7),i+=g[o+u]-(n-1e7*a),n=a,i<0?(g[o+u]=i+1e7,i=-1):(g[o+u]=i,i=0);for(;0!==i;){for(r-=1,n=0,u=0;u<p;u++)(n+=g[o+u]-1e7+m[u])<0?(g[o+u]=n+1e7,n=0):(g[o+u]=n,n=1);i+=n}v[o]=r}return g=E(g,c)[0],[f(v),f(g)]}(c,g):function(t,e){for(var r,o,n,i,u,p=t.length,a=e.length,s=[],l=[];p;)if(l.unshift(t[--p]),v(l),O(l,e)<0)s.push(0);else{n=1e7*l[(o=l.length)-1]+l[o-2],i=1e7*e[a-1]+e[a-2],o>a&&(n=1e7*(n+1)),r=Math.ceil(n/i);do{if(O(u=S(e,r),l)<=0)break;r--}while(r);s.push(r),l=d(l,u)}return s.reverse(),[f(s),f(l)]}(c,g))[0];var q=t.sign!==o.sign,M=r[1],N=t.sign;return"number"==typeof s?(q&&(s=-s),s=new p(s)):s=new u(s,q),"number"==typeof M?(N&&(M=-M),M=new p(M)):M=new u(M,N),[s,M]}function O(t,e){if(t.length!==e.length)return t.length>e.length?1:-1;for(var r=t.length-1;r>=0;r--)if(t[r]!==e[r])return t[r]>e[r]?1:-1;return 0}function B(t){var e=t.abs();return!e.isUnit()&&(!!(e.equals(2)||e.equals(3)||e.equals(5))||!(e.isEven()||e.isDivisibleBy(3)||e.isDivisibleBy(5))&&(!!e.lesser(49)||void 0))}function A(t,e){for(var r,n,i,u=t.prev(),p=u,a=0;p.isEven();)p=p.divide(2),a++;t:for(n=0;n<e.length;n++)if(!t.lesser(e[n])&&!(i=o(e[n]).modPow(p,t)).isUnit()&&!i.equals(u)){for(r=a-1;0!=r;r--){if((i=i.square().mod(t)).isUnit())return!1;if(i.equals(u))continue t}return!1}return!0}u.prototype=Object.create(i.prototype),p.prototype=Object.create(i.prototype),a.prototype=Object.create(i.prototype),u.prototype.add=function(t){var e=Y(t);if(this.sign!==e.sign)return this.subtract(e.negate());var r=this.value,o=e.value;return e.isSmall?new u(m(r,Math.abs(o)),this.sign):new u(g(r,o),this.sign)},u.prototype.plus=u.prototype.add,p.prototype.add=function(t){var e=Y(t),r=this.value;if(r<0!==e.sign)return this.subtract(e.negate());var o=e.value;if(e.isSmall){if(s(r+o))return new p(r+o);o=l(Math.abs(o))}return new u(m(o,Math.abs(r)),r<0)},p.prototype.plus=p.prototype.add,a.prototype.add=function(t){return new a(this.value+Y(t).value)},a.prototype.plus=a.prototype.add,u.prototype.subtract=function(t){var e=Y(t);if(this.sign!==e.sign)return this.add(e.negate());var r=this.value,o=e.value;return e.isSmall?w(r,Math.abs(o),this.sign):function(t,e,r){var o;return O(t,e)>=0?o=d(t,e):(o=d(e,t),r=!r),"number"==typeof(o=f(o))?(r&&(o=-o),new p(o)):new u(o,r)}(r,o,this.sign)},u.prototype.minus=u.prototype.subtract,p.prototype.subtract=function(t){var e=Y(t),r=this.value;if(r<0!==e.sign)return this.add(e.negate());var o=e.value;return e.isSmall?new p(r-o):w(o,Math.abs(r),r>=0)},p.prototype.minus=p.prototype.subtract,a.prototype.subtract=function(t){return new a(this.value-Y(t).value)},a.prototype.minus=a.prototype.subtract,u.prototype.negate=function(){return new u(this.value,!this.sign)},p.prototype.negate=function(){var t=this.sign,e=new p(-this.value);return e.sign=!t,e},a.prototype.negate=function(){return new a(-this.value)},u.prototype.abs=function(){return new u(this.value,!1)},p.prototype.abs=function(){return new p(Math.abs(this.value))},a.prototype.abs=function(){return new a(this.value>=0?this.value:-this.value)},u.prototype.multiply=function(t){var e,r,o,n=Y(t),p=this.value,a=n.value,s=this.sign!==n.sign;if(n.isSmall){if(0===a)return i[0];if(1===a)return this;if(-1===a)return this.negate();if((e=Math.abs(a))<1e7)return new u(S(p,e),s);a=l(e)}return r=p.length,o=a.length,new u(-.012*r-.012*o+15e-6*r*o>0?function t(e,r){var o=Math.max(e.length,r.length);if(o<=30)return b(e,r);o=Math.ceil(o/2);var n=e.slice(o),i=e.slice(0,o),u=r.slice(o),p=r.slice(0,o),a=t(i,p),s=t(n,u),l=t(g(i,n),g(p,u)),f=g(g(a,q(d(d(l,a),s),o)),q(s,2*o));return v(f),f}(p,a):b(p,a),s)},u.prototype.times=u.prototype.multiply,p.prototype._multiplyBySmall=function(t){return s(t.value*this.value)?new p(t.value*this.value):M(Math.abs(t.value),l(Math.abs(this.value)),this.sign!==t.sign)},u.prototype._multiplyBySmall=function(t){return 0===t.value?i[0]:1===t.value?this:-1===t.value?this.negate():M(Math.abs(t.value),this.value,this.sign!==t.sign)},p.prototype.multiply=function(t){return Y(t)._multiplyBySmall(this)},p.prototype.times=p.prototype.multiply,a.prototype.multiply=function(t){return new a(this.value*Y(t).value)},a.prototype.times=a.prototype.multiply,u.prototype.square=function(){return new u(N(this.value),!1)},p.prototype.square=function(){var t=this.value*this.value;return s(t)?new p(t):new u(N(l(Math.abs(this.value))),!1)},a.prototype.square=function(t){return new a(this.value*this.value)},u.prototype.divmod=function(t){var e=I(this,t);return{quotient:e[0],remainder:e[1]}},a.prototype.divmod=p.prototype.divmod=u.prototype.divmod,u.prototype.divide=function(t){return I(this,t)[0]},a.prototype.over=a.prototype.divide=function(t){return new a(this.value/Y(t).value)},p.prototype.over=p.prototype.divide=u.prototype.over=u.prototype.divide,u.prototype.mod=function(t){return I(this,t)[1]},a.prototype.mod=a.prototype.remainder=function(t){return new a(this.value%Y(t).value)},p.prototype.remainder=p.prototype.mod=u.prototype.remainder=u.prototype.mod,u.prototype.pow=function(t){var e,r,o,n=Y(t),u=this.value,a=n.value;if(0===a)return i[1];if(0===u)return i[0];if(1===u)return i[1];if(-1===u)return n.isEven()?i[1]:i[-1];if(n.sign)return i[0];if(!n.isSmall)throw new Error("The exponent "+n.toString()+" is too large.");if(this.isSmall&&s(e=Math.pow(u,a)))return new p(y(e));for(r=this,o=i[1];!0&a&&(o=o.times(r),--a),0!==a;)a/=2,r=r.square();return o},p.prototype.pow=u.prototype.pow,a.prototype.pow=function(t){var e=Y(t),r=this.value,o=e.value,n=BigInt(0),u=BigInt(1),p=BigInt(2);if(o===n)return i[1];if(r===n)return i[0];if(r===u)return i[1];if(r===BigInt(-1))return e.isEven()?i[1]:i[-1];if(e.isNegative())return new a(n);for(var s=this,l=i[1];(o&u)===u&&(l=l.times(s),--o),o!==n;)o/=p,s=s.square();return l},u.prototype.modPow=function(t,e){if(t=Y(t),(e=Y(e)).isZero())throw new Error("Cannot take modPow with modulus 0");var r=i[1],o=this.mod(e);for(t.isNegative()&&(t=t.multiply(i[-1]),o=o.modInv(e));t.isPositive();){if(o.isZero())return i[0];t.isOdd()&&(r=r.multiply(o).mod(e)),t=t.divide(2),o=o.square().mod(e)}return r},a.prototype.modPow=p.prototype.modPow=u.prototype.modPow,u.prototype.compareAbs=function(t){var e=Y(t),r=this.value,o=e.value;return e.isSmall?1:O(r,o)},p.prototype.compareAbs=function(t){var e=Y(t),r=Math.abs(this.value),o=e.value;return e.isSmall?r===(o=Math.abs(o))?0:r>o?1:-1:-1},a.prototype.compareAbs=function(t){var e=this.value,r=Y(t).value;return(e=e>=0?e:-e)===(r=r>=0?r:-r)?0:e>r?1:-1},u.prototype.compare=function(t){if(t===1/0)return-1;if(t===-1/0)return 1;var e=Y(t),r=this.value,o=e.value;return this.sign!==e.sign?e.sign?1:-1:e.isSmall?this.sign?-1:1:O(r,o)*(this.sign?-1:1)},u.prototype.compareTo=u.prototype.compare,p.prototype.compare=function(t){if(t===1/0)return-1;if(t===-1/0)return 1;var e=Y(t),r=this.value,o=e.value;return e.isSmall?r==o?0:r>o?1:-1:r<0!==e.sign?r<0?-1:1:r<0?1:-1},p.prototype.compareTo=p.prototype.compare,a.prototype.compare=function(t){if(t===1/0)return-1;if(t===-1/0)return 1;var e=this.value,r=Y(t).value;return e===r?0:e>r?1:-1},a.prototype.compareTo=a.prototype.compare,u.prototype.equals=function(t){return 0===this.compare(t)},a.prototype.eq=a.prototype.equals=p.prototype.eq=p.prototype.equals=u.prototype.eq=u.prototype.equals,u.prototype.notEquals=function(t){return 0!==this.compare(t)},a.prototype.neq=a.prototype.notEquals=p.prototype.neq=p.prototype.notEquals=u.prototype.neq=u.prototype.notEquals,u.prototype.greater=function(t){return this.compare(t)>0},a.prototype.gt=a.prototype.greater=p.prototype.gt=p.prototype.greater=u.prototype.gt=u.prototype.greater,u.prototype.lesser=function(t){return this.compare(t)<0},a.prototype.lt=a.prototype.lesser=p.prototype.lt=p.prototype.lesser=u.prototype.lt=u.prototype.lesser,u.prototype.greaterOrEquals=function(t){return this.compare(t)>=0},a.prototype.geq=a.prototype.greaterOrEquals=p.prototype.geq=p.prototype.greaterOrEquals=u.prototype.geq=u.prototype.greaterOrEquals,u.prototype.lesserOrEquals=function(t){return this.compare(t)<=0},a.prototype.leq=a.prototype.lesserOrEquals=p.prototype.leq=p.prototype.lesserOrEquals=u.prototype.leq=u.prototype.lesserOrEquals,u.prototype.isEven=function(){return 0==(1&this.value[0])},p.prototype.isEven=function(){return 0==(1&this.value)},a.prototype.isEven=function(){return(this.value&BigInt(1))===BigInt(0)},u.prototype.isOdd=function(){return 1==(1&this.value[0])},p.prototype.isOdd=function(){return 1==(1&this.value)},a.prototype.isOdd=function(){return(this.value&BigInt(1))===BigInt(1)},u.prototype.isPositive=function(){return!this.sign},p.prototype.isPositive=function(){return this.value>0},a.prototype.isPositive=p.prototype.isPositive,u.prototype.isNegative=function(){return this.sign},p.prototype.isNegative=function(){return this.value<0},a.prototype.isNegative=p.prototype.isNegative,u.prototype.isUnit=function(){return!1},p.prototype.isUnit=function(){return 1===Math.abs(this.value)},a.prototype.isUnit=function(){return this.abs().value===BigInt(1)},u.prototype.isZero=function(){return!1},p.prototype.isZero=function(){return 0===this.value},a.prototype.isZero=function(){return this.value===BigInt(0)},u.prototype.isDivisibleBy=function(t){var e=Y(t);return!e.isZero()&&(!!e.isUnit()||(0===e.compareAbs(2)?this.isEven():this.mod(e).isZero()))},a.prototype.isDivisibleBy=p.prototype.isDivisibleBy=u.prototype.isDivisibleBy,u.prototype.isPrime=function(t){var e=B(this);if(void 0!==e)return e;var r=this.abs(),n=r.bitLength();if(n<=64)return A(r,[2,3,5,7,11,13,17,19,23,29,31,37]);for(var i=Math.log(2)*n.toJSNumber(),u=Math.ceil(!0===t?2*Math.pow(i,2):i),p=[],a=0;a<u;a++)p.push(o(a+2));return A(r,p)},a.prototype.isPrime=p.prototype.isPrime=u.prototype.isPrime,u.prototype.isProbablePrime=function(t){var e=B(this);if(void 0!==e)return e;for(var r=this.abs(),n=void 0===t?5:t,i=[],u=0;u<n;u++)i.push(o.randBetween(2,r.minus(2)));return A(r,i)},a.prototype.isProbablePrime=p.prototype.isProbablePrime=u.prototype.isProbablePrime,u.prototype.modInv=function(t){for(var e,r,n,i=o.zero,u=o.one,p=Y(t),a=this.abs();!a.isZero();)e=p.divide(a),r=i,n=p,i=u,p=a,u=r.subtract(e.multiply(u)),a=n.subtract(e.multiply(a));if(!p.isUnit())throw new Error(this.toString()+" and "+t.toString()+" are not co-prime");return-1===i.compare(0)&&(i=i.add(t)),this.isNegative()?i.negate():i},a.prototype.modInv=p.prototype.modInv=u.prototype.modInv,u.prototype.next=function(){var t=this.value;return this.sign?w(t,1,this.sign):new u(m(t,1),this.sign)},p.prototype.next=function(){var t=this.value;return t+1<e?new p(t+1):new u(r,!1)},a.prototype.next=function(){return new a(this.value+BigInt(1))},u.prototype.prev=function(){var t=this.value;return this.sign?new u(m(t,1),!0):w(t,1,this.sign)},p.prototype.prev=function(){var t=this.value;return t-1>-e?new p(t-1):new u(r,!0)},a.prototype.prev=function(){return new a(this.value-BigInt(1))};for(var P=[1];2*P[P.length-1]<=1e7;)P.push(2*P[P.length-1]);var x=P.length,Z=P[x-1];function J(t){return Math.abs(t)<=1e7}function L(t,e,r){e=Y(e);for(var n=t.isNegative(),i=e.isNegative(),u=n?t.not():t,p=i?e.not():e,a=0,s=0,l=null,f=null,v=[];!u.isZero()||!p.isZero();)a=(l=I(u,Z))[1].toJSNumber(),n&&(a=Z-1-a),s=(f=I(p,Z))[1].toJSNumber(),i&&(s=Z-1-s),u=l[0],p=f[0],v.push(r(a,s));for(var h=0!==r(n?1:0,i?1:0)?o(-1):o(0),y=v.length-1;y>=0;y-=1)h=h.multiply(Z).add(o(v[y]));return h}u.prototype.shiftLeft=function(t){var e=Y(t).toJSNumber();if(!J(e))throw new Error(String(e)+" is too large for shifting.");if(e<0)return this.shiftRight(-e);var r=this;if(r.isZero())return r;for(;e>=x;)r=r.multiply(Z),e-=x-1;return r.multiply(P[e])},a.prototype.shiftLeft=p.prototype.shiftLeft=u.prototype.shiftLeft,u.prototype.shiftRight=function(t){var e,r=Y(t).toJSNumber();if(!J(r))throw new Error(String(r)+" is too large for shifting.");if(r<0)return this.shiftLeft(-r);for(var o=this;r>=x;){if(o.isZero()||o.isNegative()&&o.isUnit())return o;o=(e=I(o,Z))[1].isNegative()?e[0].prev():e[0],r-=x-1}return(e=I(o,P[r]))[1].isNegative()?e[0].prev():e[0]},a.prototype.shiftRight=p.prototype.shiftRight=u.prototype.shiftRight,u.prototype.not=function(){return this.negate().prev()},a.prototype.not=p.prototype.not=u.prototype.not,u.prototype.and=function(t){return L(this,t,(function(t,e){return t&e}))},a.prototype.and=p.prototype.and=u.prototype.and,u.prototype.or=function(t){return L(this,t,(function(t,e){return t|e}))},a.prototype.or=p.prototype.or=u.prototype.or,u.prototype.xor=function(t){return L(this,t,(function(t,e){return t^e}))},a.prototype.xor=p.prototype.xor=u.prototype.xor;function U(t){var e=t.value,r="number"==typeof e?e|1<<30:"bigint"==typeof e?e|BigInt(1<<30):e[0]+1e7*e[1]|1073758208;return r&-r}function T(t,e){return t=Y(t),e=Y(e),t.greater(e)?t:e}function j(t,e){return t=Y(t),e=Y(e),t.lesser(e)?t:e}function k(t,e){if(t=Y(t).abs(),e=Y(e).abs(),t.equals(e))return t;if(t.isZero())return e;if(e.isZero())return t;for(var r,o,n=i[1];t.isEven()&&e.isEven();)r=j(U(t),U(e)),t=t.divide(r),e=e.divide(r),n=n.multiply(r);for(;t.isEven();)t=t.divide(U(t));do{for(;e.isEven();)e=e.divide(U(e));t.greater(e)&&(o=e,e=t,t=o),e=e.subtract(t)}while(!e.isZero());return n.isUnit()?t:t.multiply(n)}u.prototype.bitLength=function(){var t=this;return t.compareTo(o(0))<0&&(t=t.negate().subtract(o(1))),0===t.compareTo(o(0))?o(0):o(function t(e,r){if(r.compareTo(e)<=0){var n=t(e,r.square(r)),i=n.p,u=n.e,p=i.multiply(r);return p.compareTo(e)<=0?{p:p,e:2*u+1}:{p:i,e:2*u}}return{p:o(1),e:0}}(t,o(2)).e).add(o(1))},a.prototype.bitLength=p.prototype.bitLength=u.prototype.bitLength;var z=function(t,e,r,o){r=r||"0123456789abcdefghijklmnopqrstuvwxyz",t=String(t),o||(t=t.toLowerCase(),r=r.toLowerCase());var n,i=t.length,u=Math.abs(e),p={};for(n=0;n<r.length;n++)p[r[n]]=n;for(n=0;n<i;n++){if("-"!==(l=t[n])&&(l in p&&p[l]>=u)){if("1"===l&&1===u)continue;throw new Error(l+" is not a valid digit in base "+e+".")}}e=Y(e);var a=[],s="-"===t[0];for(n=s?1:0;n<t.length;n++){var l;if((l=t[n])in p)a.push(Y(p[l]));else{if("<"!==l)throw new Error(l+" is not a valid character");var f=n;do{n++}while(">"!==t[n]&&n<t.length);a.push(Y(t.slice(f+1,n)))}}return C(a,e,s)};function C(t,e,r){var o,n=i[0],u=i[1];for(o=t.length-1;o>=0;o--)n=n.add(t[o].times(u)),u=u.times(e);return r?n.negate():n}function D(t,e){if((e=o(e)).isZero()){if(t.isZero())return{value:[0],isNegative:!1};throw new Error("Cannot convert nonzero numbers to base 0.")}if(e.equals(-1)){if(t.isZero())return{value:[0],isNegative:!1};if(t.isNegative())return{value:[].concat.apply([],Array.apply(null,Array(-t.toJSNumber())).map(Array.prototype.valueOf,[1,0])),isNegative:!1};var r=Array.apply(null,Array(t.toJSNumber()-1)).map(Array.prototype.valueOf,[0,1]);return r.unshift([1]),{value:[].concat.apply([],r),isNegative:!1}}var n=!1;if(t.isNegative()&&e.isPositive()&&(n=!0,t=t.abs()),e.isUnit())return t.isZero()?{value:[0],isNegative:!1}:{value:Array.apply(null,Array(t.toJSNumber())).map(Number.prototype.valueOf,1),isNegative:n};for(var i,u=[],p=t;p.isNegative()||p.compareAbs(e)>=0;){i=p.divmod(e),p=i.quotient;var a=i.remainder;a.isNegative()&&(a=e.minus(a).abs(),p=p.next()),u.push(a.toJSNumber())}return u.push(p.toJSNumber()),{value:u.reverse(),isNegative:n}}function R(t,e,r){var o=D(t,e);return(o.isNegative?"-":"")+o.value.map((function(t){return function(t,e){return t<(e=e||"0123456789abcdefghijklmnopqrstuvwxyz").length?e[t]:"<"+t+">"}(t,r)})).join("")}function _(t){if(s(+t)){var e=+t;if(e===y(e))return n?new a(BigInt(e)):new p(e);throw new Error("Invalid integer: "+t)}var r="-"===t[0];r&&(t=t.slice(1));var o=t.split(/e/i);if(o.length>2)throw new Error("Invalid integer: "+o.join("e"));if(2===o.length){var i=o[1];if("+"===i[0]&&(i=i.slice(1)),(i=+i)!==y(i)||!s(i))throw new Error("Invalid integer: "+i+" is not a valid exponent.");var l=o[0],f=l.indexOf(".");if(f>=0&&(i-=l.length-f-1,l=l.slice(0,f)+l.slice(f+1)),i<0)throw new Error("Cannot include negative exponent part for integers");t=l+=new Array(i+1).join("0")}if(!/^([0-9][0-9]*)$/.test(t))throw new Error("Invalid integer: "+t);if(n)return new a(BigInt(r?"-"+t:t));for(var h=[],c=t.length,g=c-7;c>0;)h.push(+t.slice(g,c)),(g-=7)<0&&(g=0),c-=7;return v(h),new u(h,r)}function Y(t){return"number"==typeof t?function(t){if(n)return new a(BigInt(t));if(s(t)){if(t!==y(t))throw new Error(t+" is not an integer.");return new p(t)}return _(t.toString())}(t):"string"==typeof t?_(t):"bigint"==typeof t?new a(t):t}u.prototype.toArray=function(t){return D(this,t)},p.prototype.toArray=function(t){return D(this,t)},a.prototype.toArray=function(t){return D(this,t)},u.prototype.toString=function(t,e){if(void 0===t&&(t=10),10!==t)return R(this,t,e);for(var r,o=this.value,n=o.length,i=String(o[--n]);--n>=0;)r=String(o[n]),i+="0000000".slice(r.length)+r;return(this.sign?"-":"")+i},p.prototype.toString=function(t,e){return void 0===t&&(t=10),10!=t?R(this,t,e):String(this.value)},a.prototype.toString=p.prototype.toString,a.prototype.toJSON=u.prototype.toJSON=p.prototype.toJSON=function(){return this.toString()},u.prototype.valueOf=function(){return parseInt(this.toString(),10)},u.prototype.toJSNumber=u.prototype.valueOf,p.prototype.valueOf=function(){return this.value},p.prototype.toJSNumber=p.prototype.valueOf,a.prototype.valueOf=a.prototype.toJSNumber=function(){return parseInt(this.toString(),10)};for(var $=0;$<1e3;$++)i[$]=Y($),$>0&&(i[-$]=Y(-$));return i.one=i[1],i.zero=i[0],i.minusOne=i[-1],i.max=T,i.min=j,i.gcd=k,i.lcm=function(t,e){return t=Y(t).abs(),e=Y(e).abs(),t.divide(k(t,e)).multiply(e)},i.isInstance=function(t){return t instanceof u||t instanceof p||t instanceof a},i.randBetween=function(t,e){var r=j(t=Y(t),e=Y(e)),o=T(t,e).subtract(r).add(1);if(o.isSmall)return r.add(Math.floor(Math.random()*o));for(var n=D(o,1e7).value,u=[],p=!0,a=0;a<n.length;a++){var s=p?n[a]:1e7,l=y(Math.random()*s);u.push(l),l<s&&(p=!1)}return r.add(i.fromArray(u,1e7,!1))},i.fromArray=function(t,e,r){return C(t.map(Y),Y(e||10),r)},i}();t.hasOwnProperty("exports")&&(t.exports=o),void 0===(r=function(){return o}.apply(e,[]))||(t.exports=r)}).call(this,r("YuTi")(t))}}]);