(window.webpackJsonp=window.webpackJsonp||[]).push([[13],{"/OsL":function(e,n,t){"use strict";var o,r,c=t("bqfS"),i=t("g4Rk"),s=t("eAL6"),a=t("vKy7"),l=(o=s._justify(s.SpaceBetween.value),r=c.style((function(e){return a.css()({borderColor:a.color(e.colors.black5),backgroundColor:a.color(e.colors.black5)})})),function(e){return i.border(i._round(s._row(o(r(e)))))});e.exports={clip:l}},"1MXd":function(e,n,t){"use strict";var o=t("BAuV"),r=t("NL7e"),c=t("jPEo"),i=t("CO8c"),s=t("5a5/"),a=t("U4xy"),l=t("YfR1"),u=t("+rx9"),d=t("IHIf"),p=t("2+07"),f=t("+O+U"),h=t("FDst"),m=t("nEj/"),b=t("DkUo"),w=t("jILx"),g=t("R8Br"),C=t("WwoG"),x=t("bqfS"),v=t("eAL6"),y=t("/OsL"),R=t("UYRu"),_=t("sfHK"),N=t("h+YZ"),S=t("vKy7"),A=t("C9r8"),k=t("bgH5"),B=t("xxbb"),E=function(e){return c.bind(d.bindAff)(p.liftEffect(d.monadEffectAff)(A.readRefMaybe(e)))((function(e){return s.for_(d.applicativeAff)(s.foldableMaybe)(e)((function(e){return d.makeAff((function(n){return function(){return o.copyNodeContents(n(new i.Right(u.unit)),(function(e){return f.error(h.message(e))(),n(new i.Right(u.unit))()}),e),d.nonCanceler}}))}))}))},L=m.unsafePerformEffect(b.lumiComponent()()()("Clip")({content:[]})((function(e){return k.bind(k.ixBindRender)(R.useTheme)((function(n){return k.bind(k.ixBindRender)(A.useRef(a.null))((function(t){var o,i=C.linkButton(x.style_(S.css()({"&:disabled":S.nested(S.css()({color:S.color(n.colors.black1)}))}))(C.onPress(c.discard(c.discardUnit)(d.bindAff)(E(t))((function(){return d.delay(5e3)})))(b.withContent(C.loadingContent([g.box(v._flex(b.withContent(v._align(v.End.value))([_.text("Copied!")])))]))([g.box(v._flex(b.withContent(v._align(v.End.value))([_.text("Copy")])))]))));return r.pure(k.applicativeRender(B.refl))(S.element(N["div'"]())({className:e.className,css:l.append(l.semigroupFn(S.semigroupStyle))(x.toCSS(y.clip))(e.css)(n),children:[S.element(N["div'"]())({className:"",css:x.toCSS((o=v._justify(v.Center.value),function(e){return v.box(o(e))}))(n),ref:t,children:e.content}),w.hspace(w.S8.value),g.box(b.withContent(v._align(v.End.value))([i]))]}))}))}))})));e.exports={clip:L,copy:E,copyNodeContents:o.copyNodeContents}},BAuV:function(e,n,t){"use strict";n.copyNodeContents=(e,n,t)=>{try{const o=(e=>{const n=document.createRange();n.selectNodeContents(e);var t=window.getSelection();return t.removeAllRanges(),t.addRange(n),t})(t);if(null!=window.navigator.clipboard)return void window.navigator.clipboard.writeText(o.toString().trim()).then(()=>{o.removeAllRanges()}).then(e,n);return window.document.execCommand("copy")?(o.removeAllRanges(),e()):n(new Error("Failed to copy"))}catch(e){n(e)}}},ugM7:function(e,n,t){"use strict";var o=t("DkUo"),r=t("5L+5"),c=t("jILx"),i=t("R8Br"),s=t("1MXd"),a=t("4hQm"),l=t("bqfS"),u=t("vKy7"),d=i.box((function(e){return{css:e.css,className:e.className,content:[o.withContent(a.paragraph_)('The Clip component wraps the provided content with a grey border and a "Copy" button, which copies the text content into the system clipboard.'),o.withContent(a.paragraph_)("If clipboard access is not allowed or not supported the text will be left highlighted, allowing the user to press ctrl+c manually. Only the plain text content is copied, not the HTML."),c.vspace(c.S24.value),r.example(s.clip((function(e){return{css:e.css,className:e.className,content:[a.paragraph(l.style_(u.css()({marginBottom:u.px(0)}))(o.withContent(a.truncate)([o.withContent(a.text)("someone@email.com")])))]}})))]}}));e.exports={docs:d}}}]);