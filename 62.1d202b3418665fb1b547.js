(window.webpackJsonp=window.webpackJsonp||[]).push([[62],{iZ0T:function(e,t,a){"use strict";var n,r,i,u,l=a("NL7e"),o=a("jPEo"),d=a("lapV"),s=a("Z/IS"),c=a("CO8c"),f=a("POE8"),p=a("5a5/"),v=a("XSw5"),m=a("ll6l"),h=a("gK7E"),y=a("H2/w"),w=a("W+DG"),b=a("YfR1"),g=a("+rx9"),U=a("a0EN"),A=a("IHIf"),B=a("VWsW"),I=a("2+07"),F=a("+O+U"),x=a("aNND"),E=a("5JIH"),S=a("SZBU"),M=a("6g2T"),N=a("5L+5"),k=a("ErWj"),q=a("+0be"),C=a("pMgY"),O=a("agFx"),L=a("7HZk"),_=a("sfHK"),z=a("avNA"),R=a("z85V"),T=a("O43b"),H=a("ZR40"),W=function(){function e(){}return e.value=new e,e}(),Z=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),J=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),K=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),P=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),V=function(){function e(){}return e.value=new e,e}(),j=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),D=E.runEffectFn2(F.log),Y=L.createComponent("UploadExample"),G=(n=function(e){return function(t){if(t instanceof W)return l.pure(U.applicativeEffect)(g.unit);if(t instanceof Z)return e.setState((function(e){return{readonly:t.value0,startUpload:e.startUpload,avatar:e.avatar,files:e.files,images:e.images}}));if(t instanceof J)return e.setStateThen((function(e){return{images:t.value0,files:e.files,avatar:e.avatar,readonly:e.readonly,startUpload:e.startUpload}}))(D("value:")(t.value0));if(t instanceof K)return e.setStateThen((function(e){return{images:e.images,files:t.value0,avatar:e.avatar,readonly:e.readonly,startUpload:e.startUpload}}))(D("value:")(t.value0));if(t instanceof P)return e.setStateThen((function(e){return{images:e.images,files:e.files,avatar:t.value0,readonly:e.readonly,startUpload:e.startUpload}}))(D("value:")(t.value0));if(t instanceof V)return A.launchAff_(y.maybe(m.map(A.functorAff)(j.create)(B.new(g.unit)))((a=m.map(A.functorAff)(v.const(W.value)),n=B.tryPut(g.unit),function(e){return a(n(e))}))(e.state.startUpload));var a,n;if(t instanceof j)return e.setState((function(e){return{readonly:e.readonly,startUpload:new y.Just(t.value0),avatar:e.avatar,files:e.files,images:e.images}}));throw new Error("Failed pattern match at Lumi.Components.Examples.Upload (line 57, column 17 - line 87, column 52): "+[t.constructor.name])}},r=o.bind(A.bindAff)(I.liftEffect(A.monadEffectAff)(x.randomRange(100)(700)))((function(e){return A.delay(e)})),i=function(e){return function(t){var a=h.round(H.size(t)),n={totalBytes:a,uploadedBytes:0};return o.discard(o.discardUnit)(A.bindAff)(r)((function(){return o.discard(o.discardUnit)(A.bindAff)(d.emit(e)(n))((function(){return o.discard(o.discardUnit)(A.bindAff)(r)((function(){return o.discard(o.discardUnit)(A.bindAff)(d.emit(e)({totalBytes:n.totalBytes,uploadedBytes:f.div(f.euclideanRingInt)(a)(8)}))((function(){return o.discard(o.discardUnit)(A.bindAff)(r)((function(){return o.discard(o.discardUnit)(A.bindAff)(d.emit(e)({totalBytes:n.totalBytes,uploadedBytes:f.div(f.euclideanRingInt)(a)(2)}))((function(){return o.discard(o.discardUnit)(A.bindAff)(r)((function(){return o.discard(o.discardUnit)(A.bindAff)(d.emit(e)({totalBytes:n.totalBytes,uploadedBytes:a}))((function(){return d.close(e)(l.pure(c.applicativeEither)(O.FileId(H.name(t))))}))}))}))}))}))}))}))}))}},u={images:[],files:[],avatar:y.Nothing.value,readonly:!1,startUpload:y.Nothing.value},L.make()(Y)({initialState:u,didMount:function(e){return A.launchAff_(m.map(A.functorAff)((t=n(e),function(e){return t(j.create(e))}))(B.empty));var t},render:function(e){return M.column_([M.column({style:R.css({maxWidth:500,padding:"20px 0"}),children:[q.labeledField({label:_.text("Readonly"),value:k.input({type:k.switch.type,autoComplete:k.switch.autoComplete,autoFocus:k.switch.autoFocus,checked:e.state.readonly?k.On.value:k.Off.value,disabled:k.switch.disabled,max:k.switch.max,min:k.switch.min,step:k.switch.step,name:k.switch.name,onBlur:k.switch.onBlur,onChange:T.handler(z.targetChecked)((c=n(e),f=y.fromMaybe(!1),function(e){return c(Z.create(f(e)))})),onFocus:k.switch.onFocus,onKeyUp:k.switch.onKeyUp,pattern:k.switch.pattern,placeholder:k.switch.placeholder,readOnly:k.switch.readOnly,required:k.switch.required,size:k.switch.size,style:k.switch.style,testId:k.switch.testId,value:k.switch.value,variant:k.switch.variant}),validationError:y.Nothing.value,required:q.Neither.value,forceTopLabel:!1,style:R.css({})})]}),C.h2_("Images"),N.example(M.column({style:R.css({alignSelf:"stretch"}),children:[O.upload({allowMultiple:O.defaults.allowMultiple,backend:{fetch:function(e){return l.pure(A.applicativeAff)({id:e,name:e,previewUri:y.Nothing.value})},upload:function(e){return d.produceAff((function(t){return i(t)(e)}))}},disabled:O.defaults.disabled,name:O.defaults.name,onBlur:O.defaults.onBlur,onChange:(u=n(e),function(e){return u(J.create(e))}),onFocus:O.defaults.onFocus,readonly:e.state.readonly,required:O.defaults.required,testId:O.defaults.testId,value:e.state.images,variant:O.Images.value})]})),C.h2_("Files"),N.example(M.column({style:R.css({alignSelf:"stretch"}),children:[O.upload({allowMultiple:O.defaults.allowMultiple,backend:{fetch:function(e){return l.pure(A.applicativeAff)({id:e,name:e,previewUri:y.Nothing.value})},upload:function(t){return d.produceAff((function(a){return o.discard(o.discardUnit)(A.bindAff)(p.foldMap(p.foldableMaybe)(A.monoidAff(w.monoidUnit))(B.read)(e.state.startUpload))((function(){return o.discard(o.discardUnit)(A.bindAff)(m.void(A.functorAff)(p.foldMap(p.foldableMaybe)(A.monoidAff(y.monoidMaybe(b.semigroupUnit)))(B.tryTake)(e.state.startUpload)))((function(){return i(a)(t)}))}))}))}},disabled:O.defaults.disabled,name:O.defaults.name,onBlur:O.defaults.onBlur,onChange:(r=n(e),function(e){return r(K.create(e))}),onFocus:O.defaults.onFocus,readonly:e.state.readonly,required:O.defaults.required,testId:O.defaults.testId,value:e.state.files,variant:O.Files.value}),S.button({accessibilityLabel:S.primary.accessibilityLabel,color:S.primary.color,onPress:T.handler_(n(e)(V.value)),size:S.primary.size,style:S.primary.style,testId:S.primary.testId,title:"Upload",type:S.primary.type,buttonState:S.primary.buttonState})]})),C.h2_("Avatar"),N.example(M.column({style:R.css({alignSelf:"stretch"}),children:[O.upload({allowMultiple:O.defaults.allowMultiple,backend:{fetch:function(e){return l.pure(A.applicativeAff)({id:e,name:e,previewUri:y.Nothing.value})},upload:function(e){return d.produceAff((function(t){return i(t)(e)}))}},disabled:O.defaults.disabled,name:O.defaults.name,onBlur:O.defaults.onBlur,onChange:(a=n(e),function(e){return a(P.create(s.head(e)))}),onFocus:O.defaults.onFocus,readonly:e.state.readonly,required:O.defaults.required,testId:O.defaults.testId,value:y.maybe([])(l.pure(l.applicativeArray))(e.state.avatar),variant:O.Avatar.value})]})),C.h2_("Logo"),N.example(M.column({style:R.css({alignSelf:"stretch"}),children:[O.upload({allowMultiple:O.defaults.allowMultiple,backend:{fetch:function(e){return l.pure(A.applicativeAff)({id:e,name:e,previewUri:y.Nothing.value})},upload:function(e){return d.produceAff((function(t){return i(t)(e)}))}},disabled:O.defaults.disabled,name:O.defaults.name,onBlur:O.defaults.onBlur,onChange:(t=n(e),function(e){return t(P.create(s.head(e)))}),onFocus:O.defaults.onFocus,readonly:e.state.readonly,required:O.defaults.required,testId:O.defaults.testId,value:y.maybe([])(l.pure(l.applicativeArray))(e.state.avatar),variant:O.Logo.value})]}))]);var t,a,r,u,c,f}})(g.unit));e.exports={component:Y,NoOp:W,SetReadonly:Z,ImageEx:J,FileEx:K,AvatarEx:P,StartUpload:V,InitializeUploadBuffer:j,docs:G,debug:D}}}]);