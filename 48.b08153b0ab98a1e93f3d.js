(window.webpackJsonp=window.webpackJsonp||[]).push([[48],{En4m:function(t,e,n){"use strict";var a=n("4Bm9"),o=n("IPld"),r=n("H8k5"),s=n("Hnrf"),i=n("YrJM"),u=n("sAO9"),c=n("rkps"),h=n("UhtL"),l=n("C3n0"),p=n("Woux"),d=n("mjQF"),y=n("T54g"),m=n("Fwyz"),b=n("7RCK"),f=n("qajM"),g=n("FFyt"),w=n("Nzb8"),v=f.createComponent("TabExample"),R=i.flip(f.element)({})(b.withRouter(f.toReactComponent()(a.identity(a.categoryFn))(v)({render:function(t){return e=t.props,p.column_([m.h2_("Demo 1"),d.example(y.tabs({currentLocation:w.URL("#"+(e.location.pathname+(e.location.search+e.location.hash))),useHash:!0,navigate:new c.Just(function(t){var n=y.urlParts(t),a=n.path+(n.query+(n.hash.path+n.hash.query)),o=c.fromMaybe("")(i.flip(r.index)(1)(l.split("#")(a)));return function(){return e.history.push(w.URL(o))}}),queryKey:"demo1",style:g.css({}),tabStyle:g.css({}),selectedTabStyle:g.css({}),tabs:u.mapFlipped(u.functorArray)(r.range(1)(6))(function(t){var e="Tab with a long title "+h.show(h.showInt)(t);return{content:function(t){return f.empty},id:l.toLower(e),label:e,count:u.voidRight(c.functorMaybe)(7*((7*(t-1|0)|0)*s.mod(s.euclideanRingInt)(t-1|0)(4)|0)|0)(o.guard(c.monadZeroMaybe)(1!==s.mod(s.euclideanRingInt)(t)(3))),testId:c.Nothing.value}})})),m.h2_("Demo 2"),d.example(y.tabs({currentLocation:w.URL("#"+(e.location.pathname+(e.location.search+e.location.hash))),useHash:!0,navigate:new c.Just(function(t){var n=y.urlParts(t),a=n.path+(n.query+(n.hash.path+n.hash.query)),o=c.fromMaybe("")(i.flip(r.index)(1)(l.split("#")(a)));return function(){return e.history.push(w.URL(o))}}),queryKey:"demo2",style:g.css({}),tabStyle:g.css({}),selectedTabStyle:g.css({}),tabs:u.mapFlipped(u.functorArray)(r.range(1)(6))(function(t){var e="Tab"+h.show(h.showInt)(t);return{content:function(t){return f.empty},id:l.toLower(e),label:e,count:u.voidRight(c.functorMaybe)(s.div(s.euclideanRingInt)(t)(2))(o.guard(c.monadZeroMaybe)(1===s.mod(s.euclideanRingInt)(t)(4))),testId:c.Nothing.value}})}))]);var e}})));t.exports={component:v,docs:R};n("sygH"),n("xVTD"),n("yb+h")}}]);