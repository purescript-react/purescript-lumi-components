(window.webpackJsonp=window.webpackJsonp||[]).push([[14],{Yx4M:function(e,n,t){"use strict";n.qrcode_=t("D1Df"),n.generateSVGUrl=e=>()=>{const n=e.current;if(null==n)throw new Error("Cannot save the contents of an empty ref");const t=n.querySelector("svg");if(null==t)throw new Error("Inner SVG node not found");const r=(new XMLSerializer).serializeToString(t),o=new Blob([r],{type:"image/svg+xml;charset=utf-8"}),a=URL.createObjectURL(o);return{url:a,dispose:()=>URL.revokeObjectURL(a)}}},fsxi:function(e,n,t){"use strict";var r=t("NL7e"),o=t("YKFa"),a=t("5a5/"),u=t("XSw5"),i=t("H2/w"),c=t("+rx9"),s=t("a0EN"),l=t("nEj/"),f=t("5L+5"),d=t("ErWj"),p=t("jILx"),x=t("pMgY"),m=t("R8Br"),v=t("+dAI"),w=t("twnB"),g=t("bqfS"),b=t("g4Rk"),_=t("eAL6"),L=t("avNA"),R=t("vKy7"),C=t("C9r8"),h=t("bgH5"),y=t("xxbb"),E=l.unsafePerformEffect(C.component("QRCode")((function(e){return h.bind(h.ixBindRender)(w.useQRCode(w.ECLLow.value)(e.value))((function(e){return r.pure(h.applicativeRender(y.refl))(m.box(_._align(_.Center.value)((function(n){return{css:n.css,className:n.className,content:[e.qrcode(b.border(b._round(g.style_(R.css()({padding:R.px(16),width:R.px(140)}))(o.identity(o.categoryFn))))),p.vspace(p.S8.value),v.link((function(n){return{css:n.css,className:n.className,href:i.fromMaybe("")(e.url),navigate:n.navigate,tabIndex:n.tabIndex,target:n.target,rel:n.rel,download:new i.Just("qrcode.svg"),ariaLabel:n.ariaLabel,content:[x.subsectionHeader_("Download SVG")],className:n.className}}))]}}))))}))}))),S=l.unsafePerformEffect(C.component("QRCodeExample")((function(e){return h.bind(h.ixBindRender)(C.useState("https://www.lumi.com"))((function(e){return r.pure(h.applicativeRender(y.refl))(m.box((function(n){return{css:n.css,className:n.className,content:[d.input({type:d.text_.type,autoComplete:d.text_.autoComplete,autoFocus:d.text_.autoFocus,checked:d.text_.checked,disabled:d.text_.disabled,max:d.text_.max,min:d.text_.min,maxLength:d.text_.maxLength,minLength:d.text_.minLength,step:d.text_.step,name:d.text_.name,onBlur:d.text_.onBlur,onChange:L.capture(L.targetValue)(a.traverse_(s.applicativeEffect)(a.foldableMaybe)((function(n){return e.value1(u.const(n))}))),onFocus:d.text_.onFocus,onKeyUp:d.text_.onKeyUp,pattern:d.text_.pattern,placeholder:d.text_.placeholder,readOnly:d.text_.readOnly,required:d.text_.required,size:d.text_.size,style:d.text_.style,testId:d.text_.testId,value:e.value0,variant:d.text_.variant}),p.vspace(p.S24.value),f.example(E({value:e.value0}))]}})))}))})))(c.unit);e.exports={docs:S,qrcodeExample:E}},twnB:function(e,n,t){"use strict";var r,o=t("Yx4M"),a=t("NL7e"),u=t("6nUn"),i=t("H2/w"),c=t("5Iaq"),s=t("U4xy"),l=t("YfR1"),f=t("FcYH"),d=t("nEj/"),p=t("DkUo"),x=t("bqfS"),m=t("eAL6"),v=t("UYRu"),w=t("7HZk"),g=t("h+YZ"),b=t("vKy7"),_=t("C9r8"),L=t("bgH5"),R=t("xxbb"),C=function(){function e(){}return e.value=new e,e}(),h=function(){function e(){}return e.value=new e,e}(),y=function(){function e(){}return e.value=new e,e}(),E=function(){function e(){}return e.value=new e,e}(),S=(r=x.style_(b.css()({label:b.str("qrcode")})),function(e){return m.box(r(e))}),q=new c.Newtype((function(e){return e}),(function(e){return e})),N=function(e){if(e instanceof C)return"L";if(e instanceof h)return"M";if(e instanceof y)return"Q";if(e instanceof E)return"H";throw new Error("Failed pattern match at Lumi.Components2.QRCode (line 89, column 29 - line 93, column 17): "+[e.constructor.name])},U=new u.Eq((function(e){return function(n){return e instanceof C&&n instanceof C||(e instanceof h&&n instanceof h||(e instanceof y&&n instanceof y||e instanceof E&&n instanceof E))}}));e.exports={useQRCode:function(e){return function(n){return L.coerceHook(q)(L.bind(L.ixBindRender)(_.useRef(s.null))((function(t){return L.bind(L.ixBindRender)(_.useMemo(f.eqTuple(u.eqString)(U))(new f.Tuple(n,e))((function(r){return d.unsafePerformEffect(p.lumiComponent()()()("QRCode")({})((function(r){return L.bind(L.ixBindRender)(v.useTheme)((function(u){return a.pure(L.applicativeRender(R.refl))(b.element(g["div'"]())({children:[w.element(o.qrcode_)({value:n,level:N(e),renderAs:"svg",xmlns:"http://www.w3.org/2000/svg",size:s.null,bgColor:"rgba(255,255,255,0.0)",fgColor:"rgba(0,0,0,1.0)"})],ref:t,className:r.className,css:l.append(l.semigroupFn(b.semigroupStyle))(x.toCSS(S))(r.css)(u)}))}))})))})))((function(e){return L.bind(L.ixBindRender)(_.useState(i.Nothing.value))((function(n){return L.discard(L.ixBindRender)(_.useEffect(_.eqUnsafeReference)(e)((function(){var e=o.generateSVGUrl(t)();return n.value1((function(n){return i.Just.create(e.url)}))(),e.dispose})))((function(){return a.pure(L.applicativeRender(R.refl))({qrcode:e,url:n.value0})}))}))}))})))}},ECLLow:C,ECLMedium:h,ECLQuartile:y,ECLHigh:E,errorCorrectLevelToString:N,ntUseQRCode:q,eqErrorCorrectLevel:U,qrcode_:o.qrcode_,generateSVGUrl:o.generateSVGUrl}}}]);