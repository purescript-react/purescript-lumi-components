(window.webpackJsonp=window.webpackJsonp||[]).push([[15],{fTYT:function(e,t,i){"use strict";var n,o=i("Z/IS"),a=i("5a5/"),r=i("XSw5"),s=i("H2/w"),l=i("W+DG"),u=i("5Iaq"),c=i("RqP8"),m=i("+rx9"),d=i("+O+U"),h=i("YKuY"),p=i("6g2T"),f=i("5L+5"),g=i("pMgY"),w=i("7HZk"),b=i("h+YZ"),v=i("LQUV"),y=(n=[{title:"Poly Mailers",subtitle:'14.50" × 19.00"',href:s.Just.create("lumi.com"),image:new s.Just("https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg")},{title:"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec rhoncus neque in consequat fermentum",subtitle:'9.00" × 5.00"',href:s.Just.create("lumi.com/items"),image:new s.Just("https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg")},{title:"Poly Mailers (no image)",subtitle:'14.50" × 19.00"',href:s.Nothing.value,image:s.Nothing.value},{title:"Random product",subtitle:'4.50" × 12.30"',href:s.Just.create("lumi.com"),image:s.Nothing.value},{title:"This text is so large it doesn't fit in a single line.",subtitle:'11.00" × 7.00"',href:s.Nothing.value,image:new s.Just("https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg")},{title:"Random product 2",subtitle:"This is a very long subtitle. I wonder what will happen to it?",href:s.Nothing.value,image:s.Nothing.value}],w.make()(w.createComponent("CardGridExample"))({initialState:{selected:[]},render:function(e){return p.column_([g.h2_("Responsive item grid"),g.p_("* Resize the window to see how the component responds."),f.example(h.cardGrid({items:o.mapWithIndex(function(e){return function(t){return{key:c.show(c.showInt)(e),title:t.title,subtitle:t.subtitle,href:t.href,children:a.foldMap(a.foldableMaybe)(l.monoidArray)(function(e){return[b.img()({src:e})]})(t.image)}}})(n),onNavigate:(w=u.un(v.newtypeURL)(v.URL),function(e){return d.log(w(e))}),selection:s.Nothing.value})),g.h2_("Item grid with few items"),f.example(h.cardGrid({items:o.mapWithIndex(function(e){return function(t){return{key:c.show(c.showInt)(e),title:t.title,subtitle:t.subtitle,href:t.href,children:a.foldMap(a.foldableMaybe)(l.monoidArray)(function(e){return[b.img()({src:e})]})(t.image)}}})(o.take(2)(n)),onNavigate:(m=u.un(v.newtypeURL)(v.URL),function(e){return d.log(m(e))}),selection:s.Nothing.value})),g.h2_("Responsive, selectable item grid"),g.p_("* Resize the window to see how the component responds. Selection boxes are always visible on small screens."),f.example(h.cardGrid({items:o.mapWithIndex(function(e){return function(t){return{key:c.show(c.showInt)(e),title:t.title,subtitle:t.subtitle,href:t.href,children:a.foldMap(a.foldableMaybe)(l.monoidArray)(function(e){return[b.img()({src:e})]})(t.image)}}})(n),onNavigate:(i=u.un(v.newtypeURL)(v.URL),function(e){return d.log(i(e))}),selection:new s.Just({selectedKeys:e.state.selected,onSelect:(t=r.flip(function(e){return function(e){return{selected:e}}}),function(i){return e.setState(t(i))})})}))]);var t,i,m,w}})(m.unit));e.exports={docs:y}}}]);