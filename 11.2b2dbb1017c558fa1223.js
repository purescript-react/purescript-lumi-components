(window.webpackJsonp=window.webpackJsonp||[]).push([[11],{"9+TH":function(e,t,n){"use strict";var r=n("veZx"),i=n("6nUn"),a=n("ij/N"),o=new i.Eq((function(e){return function(t){return e===t}})),u=new a.Ord((function(){return o}),(function(e){return function(t){return a.compare(a.ordInt)(e)(t)}})),l=new i.Eq((function(e){return function(t){return e===t}})),s=new a.Ord((function(){return l}),(function(e){return function(t){return a.compare(a.ordInt)(e)(t)}}));e.exports={eqTimeoutId:o,ordTimeoutId:u,eqIntervalId:l,ordIntervalId:s,setTimeout:r.setTimeout,clearTimeout:r.clearTimeout,setInterval:r.setInterval,clearInterval:r.clearInterval}},"HQW+":function(e,t,n){"use strict";var r=n("5a5/"),i=n("DkUo"),a=n("5L+5"),o=n("jILx"),u=n("pMgY"),l=n("rQua"),s=n("7HZk"),c=r.intercalate(r.foldableArray)(s.monoidJSX)(o.vspace(o.S16.value))([u.h4_("Image default (will respect image's aspect ratio)"),a.example(i.withContent(l.image)("http://via.placeholder.com/640x360")),u.h4_("Image + resize { width: 120px, height: 40px }, respects image's aspect ratio & clips overflow"),a.example(l.image(i.withContent(l.resize({width:120,height:40}))("http://via.placeholder.com/640x360"))),u.h4_("Thumbnail default (will always have a square aspect ratio)"),a.example(i.withContent(l.thumbnail)("http://via.placeholder.com/640x360")),u.h4_("Thumbnail + resize 48px"),a.example(l.thumbnail(i.withContent(l.resizeSquare(80))("https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"))),u.h4_("Thumbnail + small"),a.example(l.thumbnail(i.withContent(l.small)("https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"))),u.h4_("Thumbnail + medium"),a.example(l.thumbnail(i.withContent(l.medium)("https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"))),u.h4_("Thumbnail + large"),a.example(l.thumbnail(i.withContent(l.large)("https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"))),u.h4_("Thumbnail + extra large"),a.example(l.thumbnail(i.withContent(l.extraLarge)("https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"))),u.h4_("Thumbnail + round"),a.example(l.thumbnail(l.round(i.withContent(l.extraLarge)("https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg")))),u.h4_("Placeholders (can be overriden)"),a.example(l.image(i.withContent(l.resize({width:900,height:50}))(""))),a.example(l.thumbnail(i.withContent(l.medium)("")))]);e.exports={docs:c}},rQua:function(e,t,n){"use strict";var r,i,a=n("NL7e"),o=n("6nUn"),u=n("5a5/"),l=n("H2/w"),s=n("W+DG"),c=n("U4xy"),m=n("ij/N"),h=n("YfR1"),d=n("aa8G"),p=n("Qku0"),f=n("fGjL"),x=n("+rx9"),g=n("a0EN"),w=n("9+TH"),v=n("nEj/"),b=n("DkUo"),y=n("OiCe"),_=n("Lyn7"),I=n("R8Br"),C=n("bqfS"),T=n("eAL6"),N=n("UYRu"),R=n("7HZk"),q=n("h+YZ"),S=n("z85V"),z=n("vKy7"),E=n("O43b"),j=n("C9r8"),L=n("bgH5"),B=n("xxbb"),U=n("B4d4"),k=function(){function e(){}return e.value=new e,e}(),H=function(){function e(){}return e.value=new e,e}(),J=C.style_(z.css()({width:z.px(40),height:z.px(40)})),M=z.css()({borderRadius:z.percent(50)}),Z=C.style_(M),O=(C.style_(z.merge([z.css()({width:z.px(24),height:z.px(24)}),M])),C.style_(z.merge([z.css()({width:z.px(30),height:z.px(30)}),M])),C.style_(z.merge([z.css()({width:z.px(36),height:z.px(36)}),M])),C.style_(z.css()({width:z.px(72),height:z.px(72)}))),Q=(r={component:H.value,content:"",placeholder:l.Nothing.value},i=function(e){return z.css()({boxSizing:z.borderBox,overflow:z.hidden,display:z.flex,flexFlow:z.column,alignItems:z.center,border:z.str("1px solid"),borderColor:z.color(e.colors.black4)})},v.unsafePerformEffect(b.lumiComponent()()()("Image")(r)((function(e){return L.bind(L.ixBindRender)(N.useTheme)((function(t){return L.bind(L.ixBindRender)(j["useState'"](!1))((function(n){return L.bind(L.ixBindRender)(j.useRef(c.null))((function(r){return L.bind(L.ixBindRender)(j.useState({width:20,height:20}))((function(v){return L.discard(L.ixBindRender)(j.useLayoutEffect(o.eqUnit)(x.unit)((function(){var e=j.readRefMaybe(r)();if(e instanceof l.Nothing)return s.mempty(g.monoidEffect(g.monoidEffect(s.monoidUnit)))();if(e instanceof l.Just){var t=f.traverse(f.traversableMaybe)(g.applicativeEffect)(U.getBoundingClientRect)(U.fromNode(e.value0))(),n={height:l.maybe(0)((function(e){return e.height+2}))(t),width:l.maybe(0)((function(e){return e.width+2}))(t)};if(o.notEq(o.eqRec()(o.eqRowCons(o.eqRowCons(o.eqRowNil)()(new p.IsSymbol((function(){return"width"})))(o.eqNumber))()(new p.IsSymbol((function(){return"height"})))(o.eqNumber)))(n)(v.value0)){var i=w.setTimeout(10)(v.value1((function(e){return n})))();return w.clearTimeout(i)}return s.mempty(g.monoidEffect(g.monoidEffect(s.monoidUnit)))()}throw new Error("Failed pattern match at Lumi.Components2.Image (line 63, column 9 - line 78, column 21): "+[e.constructor.name])})))((function(){return a.pure(L.applicativeRender(B.refl))(z.element(q["div'"]())({ref:r,children:[d.null(e.content)?l.fromMaybe(_.placeholderSvg)(e.placeholder):I.column(T._flex(T._align(T.Center.value)(b.withContent(T._justify(T.Center.value))([s.guard(R.monoidJSX)(!n.value0)((o=l.fromMaybe(20)(u.minimum(m.ordNumber)(u.foldableArray)([v.value0.height,v.value0.width])),y.loader({style:S.css({width:.25*o,height:.25*o,borderWidth:"2px"}),testId:c.toNullable(l.Nothing.value)}))),z.element(q["img'"]())({src:e.content,className:"",css:z.css()({maxWidth:z.percent(100)}),onLoad:E.handler_(n.value1(!0)),onError:E.handler_(n.value1(!0))})]))))],className:e.className,css:h.append(z.semigroupStyle)(i(t))(e.css(t))}));var o}))}))}))}))}))})))),W=C.style_(z.css()({width:z.px(140),height:z.px(140)})),Y=C.style_(z.css()({width:z.px(56),height:z.px(56)})),A=Y,D=function(){var e={component:k.value,content:"",placeholder:l.Nothing.value};return v.unsafePerformEffect(b.lumiComponent()()()("Thumbnail")(e)((function(e){return a.pure(L.applicativeRender(B.refl))(Q((function(t){return{css:h.append(h.semigroupFn(z.semigroupStyle))(C.toCSS(Y))(e.css),className:t.className,component:t.component,content:e.content,placeholder:e.placeholder}})))})))}();e.exports={image:Q,thumbnail:D,small:J,medium:A,large:O,extraLarge:W,resize:function(e){return C.style((function(t){return z.css()({width:z.px(e.width),height:z.px(e.height)})}))},resizeSquare:function(e){return C.style((function(t){return z.css()({width:z.px(e),height:z.px(e)})}))},round:Z}},veZx:function(e,t,n){"use strict";t.setTimeout=function(e){return function(t){return function(){return setTimeout(t,e)}}},t.clearTimeout=function(e){return function(){clearTimeout(e)}},t.setInterval=function(e){return function(t){return function(){return setInterval(t,e)}}},t.clearInterval=function(e){return function(){clearInterval(e)}}}}]);