(window.webpackJsonp=window.webpackJsonp||[]).push([[8],{"/OsL":function(e,n,t){"use strict";var r,o,c=t("bqfS"),i=t("g4Rk"),s=t("eAL6"),u=t("vKy7"),a=(r=s._justify(s.SpaceBetween.value),o=c.style((function(e){return u.css()({borderColor:u.color(e.colors.black5),backgroundColor:u.color(e.colors.black5)})})),function(e){return i.border(i._round(s._row(r(o(e)))))});e.exports={clip:a}},"1MXd":function(e,n,t){"use strict";var r=t("BAuV"),o=t("NL7e"),c=t("jPEo"),i=t("6nUn"),s=t("5a5/"),u=t("W+DG"),a=t("5Iaq"),l=t("U4xy"),f=t("YfR1"),d=t("FcYH"),p=t("a0EN"),v=t("IHIf"),b=t("2+07"),m=t("+O+U"),y=t("FDst"),x=t("5JIH"),w=t("nEj/"),h=t("DkUo"),g=t("jILx"),R=t("R8Br"),C=t("WwoG"),N=t("bqfS"),S=t("eAL6"),k=t("/OsL"),A=t("UYRu"),B=t("sfHK"),E=t("h+YZ"),H=t("vKy7"),L=t("C9r8"),_=t("xDnj"),T=t("bgH5"),j=t("g47h"),I=t("xxbb"),U=function(e){return e},q=new a.Newtype((function(e){return e}),U),F=function(e){return T.coerceHook(q)(T.bind(T.ixBindRender)(j.useResetToken)((function(n){return T.bind(T.ixBindRender)(L.useState(!1))((function(t){var u=function(){var o=L.readRefMaybe(e)();return s.for_(p.applicativeEffect)(s.foldableMaybe)(o)(x.runEffectFn3(r.copyNodeContents)((function(){return t.value1((function(e){return!0}))(),n.value1()}))((function(e){return m.error(y.message(e))()})))()};return T.discard(T.ixBindRender)(_.useAff(d.eqTuple(j.eqResetToken)(i.eqBoolean))(new d.Tuple(n.value0,t.value0))(o.when(v.applicativeAff)(t.value0)(c.discard(c.discardUnit)(v.bindAff)(v.delay(5e3))((function(){return b.liftEffect(v.monadEffectAff)(t.value1((function(e){return!1})))})))))((function(){return o.pure(T.applicativeRender(I.refl))({copied:t.value0,copy:u})}))}))})))},D=w.unsafePerformEffect(h.lumiComponent()()()("Clip")({content:[]})((function(e){return T.bind(T.ixBindRender)(A.useTheme)((function(n){return T.bind(T.ixBindRender)(L.useRef(l.null))((function(t){return T.bind(T.ixBindRender)(F(t))((function(r){var c,i=C.button(C._linkStyle(N.style_(H.merge([H.css()({marginLeft:H.prop(g.isStylePropertySpace)(g.S16.value),lineHeight:H.str("1.2")}),u.guard(H.monoidStyle)(r.copied)(H.css()({color:H.color(n.colors.black1),"&:hover":H.nested(H.css()({textDecoration:H.none}))}))]))((function(e){return{css:e.css,className:e.className,content:[B.text(r.copied?"Copied!":"Copy")],onPress:r.copy,kind:e.kind,accessibilityLabel:e.accessibilityLabel,color:e.color,size:e.size,state:e.state,type:e.type}}))));return o.pure(T.applicativeRender(I.refl))(H.element(E["div'"]())({className:e.className,css:f.append(f.semigroupFn(H.semigroupStyle))(N.toCSS(k.clip))(e.css)(n),children:[H.element(E["div'"]())({className:"",css:f.append(f.semigroupFn(H.semigroupStyle))(N.toCSS((c=S._justify(S.Center.value),function(e){return S.box(c(e))})))(e.css)(n),ref:t,children:e.content}),R.box(S._justify(S.Center.value)((function(e){return{css:e.css,className:e.className,content:[i]}})))]}))}))}))}))})));e.exports={clip:D,UseClip:U,useClip:F,ntUseClip:q,copyNodeContents:r.copyNodeContents}},BAuV:function(e,n,t){"use strict";n.copyNodeContents=(e,n,t)=>{try{const r=(e=>{const n=document.createRange();n.selectNodeContents(e);var t=window.getSelection();return t.removeAllRanges(),t.addRange(n),t})(t);if(null!=window.navigator.clipboard)return void window.navigator.clipboard.writeText(r.toString().trim()).then(()=>{r.removeAllRanges()}).then(e,n);return window.document.execCommand("copy")?(r.removeAllRanges(),e()):n(new Error("Failed to copy"))}catch(e){n(e)}}},g47h:function(e,n,t){"use strict";var r=t("NL7e"),o=t("6nUn"),c=t("FcYH"),i=t("nEj/"),s=t("C9r8"),u=t("bgH5"),a=t("xxbb"),l=t("jNIV"),f=new o.Eq(l.unsafeRefEq),d=function(e){return{}},p=i.unsafePerformEffect(d),v=u.bind(u.ixBindRender)(s.useState(p))((function(e){return r.pure(u.applicativeRender(a.refl))(new c.Tuple(e.value0,(function(){var n={};return e.value1((function(e){return n}))()})))}));e.exports={useResetToken:v,eqResetToken:f}},ugM7:function(e,n,t){"use strict";var r=t("5L+5"),o=t("jILx"),c=t("pMgY"),i=t("R8Br"),s=t("1MXd"),u=i.box((function(e){return{css:e.css,className:e.className,content:[c.p_('The Clip component wraps the provided content with a grey border and a "Copy" button, which copies the text content into the system clipboard.'),c.p_("If clipboard access is not allowed or not supported the text will be left highlighted, allowing the user to press ctrl+c manually. Only the plain text content is copied, not the HTML."),o.vspace(o.S24.value),r.example(s.clip((function(e){return{css:e.css,className:e.className,content:[c.body_("someone@email.com")]}}))),c.p_("The Clip behavior is also available as a React hook.")]}}));e.exports={docs:u}},xDnj:function(e,n,t){"use strict";var r=t("NL7e"),o=t("XSw5"),c=t("H2/w"),i=t("5Iaq"),s=t("IHIf"),u=t("FDst"),a=t("C9r8"),l=t("bgH5"),f=t("xxbb"),d=function(e){return e},p=new i.Newtype((function(e){return e}),d);e.exports={UseAff:d,useAff:function(e){return function(n){return function(t){return l.coerceHook(p)(l.bind(l.ixBindRender)(a.useState(c.Nothing.value))((function(i){return l.discard(l.ixBindRender)(a.useEffect(e)(n)((function(){i.value1(o.const(c.Nothing.value))();var e=s.runAff((function(e){return i.value1(o.const(c.Just.create(e)))}))(t)();return s.launchAff_(s.killFiber(u.error("Effect hook discarded."))(e))})))((function(){return r.pure(l.applicativeRender(f.refl))(i.value0)}))})))}}},ntUseAff:p}}}]);