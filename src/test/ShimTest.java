package test;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.OutputStreamWriter;
import java.io.Reader;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class ShimTest
{
    public ShimTest() throws ScriptException, FileNotFoundException
    {
        ScriptEngineManager engineManager = new ScriptEngineManager();
        ScriptEngine engine = engineManager.getEngineByName("Clojure");

        engine.getContext().setAttribute("appInstance", new TestAppInstance(), ScriptContext.ENGINE_SCOPE);
        engine.getContext().setAttribute("incomingCall", new TestCall(), ScriptContext.ENGINE_SCOPE);
        engine.getContext().setAttribute("callFactory", new Object(), ScriptContext.ENGINE_SCOPE);
        engine.getContext().setWriter(new OutputStreamWriter(System.err));
        
        Reader input = new FileReader("tropo.clj");
        engine.eval(input);

        System.exit(0);
    }

    public static void main(String[] args) throws Exception
    {
        ShimTest test = new ShimTest();
    }
}
