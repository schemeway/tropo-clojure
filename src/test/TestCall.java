package test;

import java.util.HashMap;
import java.util.Map;

public class TestCall
{
    public String getCallerId()
    {
        return "dominique";
    }

    public String getCalledId()
    {
        return "jason";
    }

    public String getCallerName()
    {
        return "Dominique Boucher";
    }

    public String getCalledName()
    {
        return "Jason Goecke";
    }
    
    public boolean isActive()
    {
        return true;
    }
    
    public void log(String message)
    {
        System.err.println("[LOG] "  + message);
    }

    public String answer(int milliseconds)
    {
        return "answered";
    }
    
    public String hangup()
    {
        return "hungup";
    }
    
    public Object prompt(String ttsOrUrl, boolean bargein, String grammar, String choiceConfidence, String choiceMode, int timeout)
    {
        log("[PROMPT] '" + ttsOrUrl + "'");
        
        Map result = new HashMap();
        result.put("concept", "concept-value");
        result.put("interpretation", "interpretation-value");
        result.put("confidence", "1.0");
        result.put("xml", "<xml></xml>");
        result.put("utterance", "i said that");
        result.put("value", "1");
        result.put("recordURL", "http://some/url");
        return result;
    }
}
