(window.webpackJsonp=window.webpackJsonp||[]).push([[19],{"4aBa":function(e,t,n){"use strict";var a,c,s,i,o=n("NL7e"),r=n("H2/w"),l=n("5Iaq"),u=n("YfR1"),m=n("nEj/"),d=n("DkUo"),p=n("bqfS"),f=n("g4Rk"),b=n("FrUb"),v=n("UYRu"),N=n("avNA"),h=n("h+YZ"),g=n("vKy7"),x=n("bgH5"),w=n("xxbb"),k=n("LQUV"),y=(a=p.style_(g.css()({appearance:g.none})),c=function(e){return b.slat(a(e))},s=function(e){return c(f._interactive(e))},i={content:[],interaction:r.Nothing.value},m.unsafePerformEffect(d.lumiComponent()()()("Slat")(i)((function(e){return x.bind(x.ixBindRender)(v.useTheme)((function(t){return o.pure(x.applicativeRender(w.refl))(function(){if(e.interaction instanceof r.Nothing)return g.element(h["div'"]())({css:u.append(u.semigroupFn(g.semigroupStyle))(p.toCSS(c))(e.css)(t),children:e.content,className:e.className});if(e.interaction instanceof r.Just&&e.interaction.value0.href instanceof r.Nothing)return g.element(h["button'"]())({css:u.append(u.semigroupFn(g.semigroupStyle))(p.toCSS(s))(e.css)(t),children:e.content,onClick:N.capture_(e.interaction.value0.onClick),tabIndex:e.interaction.value0.tabIndex,className:e.className});if(e.interaction instanceof r.Just&&e.interaction.value0.href instanceof r.Just)return g.element(h["a'"]())({css:u.append(u.semigroupFn(g.semigroupStyle))(p.toCSS(s))(e.css)(t),children:e.content,onClick:N.capture_(e.interaction.value0.onClick),tabIndex:e.interaction.value0.tabIndex,href:l.un(k.newtypeURL)(k.URL)(e.interaction.value0.href.value0),className:e.className});throw new Error("Failed pattern match at Lumi.Components2.Slat (line 43, column 12 - line 66, column 14): "+[e.interaction.constructor.name])}())}))}))));e.exports={slat:y,_interactive:function(e){return d.propsModifier((function(t){return{css:t.css,className:t.className,interaction:new r.Just(e),content:t.content}}))},_interactiveBackground:function(e){var t=d.propsModifier((function(t){return{css:t.css,className:t.className,interaction:new r.Just(e),content:t.content}})),n=p.style((function(e){return g.css()({"&:hover":g.nested(g.css()({backgroundColor:g.color(e.colors.primary4),borderColor:g.color(e.colors.black4)}))})}));return function(e){return t(n(e))}}}},FrUb:function(e,t,n){"use strict";var a,c,s,i=n("bqfS"),o=n("g4Rk"),r=n("eAL6"),l=n("vKy7"),u=(a=r._align(r.Center.value),c=r._justify(r.SpaceBetween.value),s=i.style_(l.css()({label:l.str("slat"),flex:l.str("0 0 content"),color:l.unset,backgroundColor:l.unset,textDecoration:l.unset})),function(e){return o.border(r._row(a(c(s(e)))))});e.exports={slat:u}},TBXQ:function(e,t,n){"use strict";var a,c,s=n("NL7e"),i=n("jPEo"),o=n("Z/IS"),r=n("5a5/"),l=n("H2/w"),u=n("W+DG"),m=n("U4xy"),d=n("a0EN"),p=n("nEj/"),f=n("DkUo"),b=n("5L+5"),v=n("qz/O"),N=n("jILx"),h=n("Lyn7"),g=n("pMgY"),x=n("R8Br"),w=n("4aBa"),k=n("bqfS"),y=n("g4Rk"),S=n("eAL6"),_=n("UYRu"),C=n("7HZk"),I=n("sfHK"),L=n("z85V"),E=n("vKy7"),R=n("bgH5"),B=n("xxbb"),J=n("EN5Q"),U=n("NgiC"),q=k.style_(E.css()({maxWidth:E.int(500),width:E.str("100%")})),F=function(e){return k.style_(E.css()({flexGrow:E.int(e),"&:not(:first-child)":E.nested(E.css()({marginLeft:E.prop(N.isStylePropertySpace)(N.S16.value),alignItems:E.prop(S.isStylePropertyFlexAlign)(S.End.value)}))}))},H=(a={title:u.mempty(C.monoidJSX),value:u.mempty(C.monoidJSX)},p.unsafePerformEffect(f.lumiComponent()()()("LabeledInfo")(a)((function(e){return R.bind(R.ixBindRender)(_.useTheme)((function(t){return s.pure(R.applicativeRender(B.refl))(x.box(k.style_(E.css()({label:E.str("labeledInfo")}))((function(n){return{css:e.css,className:e.className,content:[g.text({children:[e.value],className:g.body.className,color:m.notNull(t.colorNames.black),style:L.css({whiteSpace:"nowrap"}),tag:g.body.tag,testId:g.body.testId}),g.text({children:[e.title],className:g.subtext.className,color:m.notNull(t.colorNames.black1),style:L.css({whiteSpace:"nowrap"}),tag:g.subtext.tag,testId:g.subtext.testId})]}}))))}))})))),j=(c=[x.box(F(4)((function(e){return{css:e.css,className:e.className,content:[v.userLockup({name:"Xiamen, China",description:l.Nothing.value,image:h.userSvg})]}}))),H(F(1)((function(e){return{css:e.css,className:e.className,title:I.text("Lead time"),value:I.text("11 weeks")}}))),H(F(1)((function(e){return{css:e.css,className:e.className,title:I.text("Quantities"),value:I.text("500-2.5k")}})))],r.intercalate(r.foldableArray)(C.monoidJSX)(N.vspace(N.S8.value))([g.p_("Slats are stackable, bordered containers with optional interactive behavior. They do not prescribe any formatting on their content besides defaulting to a center-aligned row layout."),g.h2_("square (default), spaced list, interactive, min-width content"),b.example(C.fragment(o.replicate(3)(w.slat(y._listSpaced(w._interactive({onClick:i.bind(d.bindEffect)(J.window)(U.alert("click!")),tabIndex:1,href:l.Nothing.value})((function(e){return{css:e.css,className:e.className,content:c,interaction:e.interaction}}))))))),g.h2_("square (default), spaced list, interactive (background color), min-width content"),b.example(C.fragment(o.replicate(3)(w.slat(y._listSpaced(w._interactiveBackground({onClick:i.bind(d.bindEffect)(J.window)(U.alert("click!")),tabIndex:1,href:l.Nothing.value})((function(e){return{css:e.css,className:e.className,content:c,interaction:e.interaction}}))))))),g.h2_("round, spaced list, non-interactive, min-width 500px"),b.example(C.fragment(o.replicate(3)(w.slat(q(y._round(y._listSpaced((function(e){return{css:e.css,className:e.className,content:c,interaction:e.interaction}})))))))),g.h2_("top/bottom, compact list, interactive, min-width 500px"),b.example(C.fragment(o.replicate(9)(w.slat(w._interactive({onClick:i.bind(d.bindEffect)(J.window)(U.alert("click!")),tabIndex:1,href:l.Nothing.value})(y._topBottom(y._listCompact(q((function(e){return{css:e.css,className:e.className,content:c,interaction:e.interaction}}))))))))),g.h2_("top/bottom, compact list, interactive (background color), min-width 500px"),b.example(C.fragment(o.replicate(9)(w.slat(w._interactiveBackground({onClick:i.bind(d.bindEffect)(J.window)(U.alert("click!")),tabIndex:1,href:l.Nothing.value})(y._topBottom(y._listCompact(q((function(e){return{css:e.css,className:e.className,content:c,interaction:e.interaction}})))))))))]));e.exports={docs:j,slatExWidth:q,slatColumn:F,labeledInfo:H}}}]);