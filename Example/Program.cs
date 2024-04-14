using System.Diagnostics;
using Mana;

var m = new Mana.Mana();

var add = (double x, double y) => x + y;

Value AddEnv(Env<Value> env, List<Value> args) =>
    args switch
    {
        [Value.Num a, Value.Num b] => Value.NewNum(a.AsNum + b.AsNum),
        _ => throw new ArgumentException("Arguments must be numbers")
    };

m.set("x", Value.NewNum(2));
m.set("y", Value.NewNum(3));
m.set("add", Value.NewFuncClosure(AddEnv));

var res = m.run("add x y");

Debug.Assert(res.AsNum == 5);