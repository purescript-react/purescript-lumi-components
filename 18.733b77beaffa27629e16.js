(window.webpackJsonp=window.webpackJsonp||[]).push([[18],{"+dAI":function(e,n,t){"use strict";var a,s,o=t("NL7e"),i=t("iMOL"),r=t("XSw5"),l=t("H2/w"),c=t("W+DG"),u=t("5Iaq"),m=t("U4xy"),f=t("Qku0"),y=t("a0EN"),g=t("nEj/"),p=t("DkUo"),h=t("bqfS"),v=t("yNh5"),d=t("UYRu"),b=t("avNA"),K=t("z85V"),N=t("vKy7"),w=t("O43b"),k=t("bgH5"),C=t("xxbb"),E=t("LQUV"),S=g.unsafePerformEffect((a=N.element(K.unsafeCreateDOMComponent("a")),s={className:"",href:"",navigate:l.Nothing.value,target:l.Nothing.value,content:[]},p.lumiComponent()()()("Link")(s)((function(e){return k.bind(k.ixBindRender)(d.useTheme)((function(n){return o.pure(k.applicativeRender(C.refl))(a({css:h.toCSS(n)(e)(v.link),children:e.content,className:e.className,href:u.un(E.newtypeURL)(E.URL)(e.href),onClick:w.handler(w.merge()(w.mergeCons(new f.IsSymbol((function(){return"altKey"})))()()()()(w.mergeCons(new f.IsSymbol((function(){return"button"})))()()()()(w.mergeCons(new f.IsSymbol((function(){return"ctrlKey"})))()()()()(w.mergeCons(new f.IsSymbol((function(){return"metaKey"})))()()()()(w.mergeCons(new f.IsSymbol((function(){return"shiftKey"})))()()()()(w.mergeCons(new f.IsSymbol((function(){return"syntheticEvent"})))()()()()(w.mergeNil)))))))({button:b.button,metaKey:b.metaKey,altKey:b.altKey,ctrlKey:b.ctrlKey,shiftKey:b.shiftKey,syntheticEvent:w.syntheticEvent}))((function(n){return e.navigate instanceof l.Just&&n.button instanceof l.Just&&0===n.button.value0&&n.metaKey instanceof l.Just&&!n.metaKey.value0&&n.altKey instanceof l.Just&&!n.altKey.value0&&n.ctrlKey instanceof l.Just&&!n.ctrlKey.value0&&n.shiftKey instanceof l.Just&&!n.shiftKey.value0?function(){return w.handler(i.compose(w.semigroupoidBuilder)(b.stopPropagation)(b.preventDefault))(r.const(e.navigate.value0))(n.syntheticEvent)}:function(){return w.handler(b.stopPropagation)(c.mempty(c.monoidFn(y.monoidEffect(c.monoidUnit))))(n.syntheticEvent)}})),target:m.toNullable(e.target)}))}))}))));e.exports={link:S}},"/AYu":function(e,n,t){"use strict";var a=t("NL7e"),s=t("H2/w"),o=t("+O+U"),i=t("DkUo"),r=t("6g2T"),l=t("5L+5"),c=t("pMgY"),u=t("+dAI"),m=t("h+YZ"),f=t("z85V"),y=r.column_([l.example(m.div()({onClick:function(e){return o.log("Propagated")()},style:f.css({display:"flex",flexDirection:"column"}),children:[i.lumiElement(u.link)((function(e){return{css:e.css,className:e.className,className:e.className,content:[c.body_("Click here")],href:"#/link",navigate:a.pure(s.applicativeMaybe)(o.log("link clicked")),target:e.target}})),i.lumiElement(u.link)((function(e){return{css:e.css,className:e.className,className:e.className,content:[c.body_("This should open in a new tab")],href:"#/input",navigate:e.navigate,target:new s.Just("_blank")}}))]}))]);e.exports={docs:y}}}]);