extern crate cranelift;
extern crate cranelift_native;
extern crate cranelift_module;
extern crate cranelift_object;

use cranelift::{
    codegen::{
        settings,
        entity::EntityRef,
        ir::{
            types::*,
            AbiParam,
            UserFuncName,
            Function,
            InstBuilder,
            Signature,
        },
        isa::CallConv,
        verifier::verify_function,
        Context,
    },
    frontend::{
        FunctionBuilder,
        FunctionBuilderContext,
        Variable,
    }, prelude::isa::TargetIsa,
};
use cranelift_object::*;
use cranelift_module::*;

fn define_function(flags: &settings::Flags, module: &mut ObjectModule) -> FuncId {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns.push(AbiParam::new(I32));
    sig.params.push(AbiParam::new(I32));

    let func_id = module.declare_function("test", Linkage::Local, &sig).unwrap();
    let user_func_name = UserFuncName::user(0, func_id.as_u32());

    let mut ctx = Context::for_function(Function::with_name_signature(user_func_name, sig));
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        let x = Variable::new(0);
        let y = Variable::new(1);

        builder.declare_var(x, I32);
        builder.declare_var(y, I32);
        builder.append_block_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            // first function parameter x
            let tmp = builder.block_params(block0)[0];
            builder.def_var(x, tmp);
        }
        {
            let arg_x = builder.use_var(x);
            let arg_y = builder.use_var(y);
            let tmp = builder.ins().iadd(arg_x, arg_y);
            builder.def_var(y, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().return_(&[arg]);
        }

    }
    let res = verify_function(&ctx.func, flags);
    if let Err(errors) = res {
        panic!("Err verifying function: {}", errors);
    }
    println!("Verified function:\n{}", ctx.func.display());
     
    module.define_function(func_id, &mut ctx).unwrap();

    func_id
}

fn main() {
    let flags = settings::Flags::new(settings::builder()); 

    // BACKEND
    
    let isa_builder = cranelift_native::builder().unwrap();
    let isa = match isa_builder.finish(flags.clone()) {
        Ok(isa) => isa,
        Err(errs) => panic!("{:?}", errs),
    };
    let obj_builder = ObjectBuilder::new(isa, "twig", default_libcall_names()).unwrap();
    let mut module = ObjectModule::new(obj_builder);

    define_function(&flags, &mut module);
    
    let product = module.finish();

    if let Ok(emitted) = product.emit() {
        println!("{:?}", emitted);
    } else {
        panic!("Unable to emit module product")
    }

    // LINKER ??
    
}
