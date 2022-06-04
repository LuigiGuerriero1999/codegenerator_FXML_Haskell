package com.example.testboy;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//lightweight String templating method
//src= https://stackoverflow.com/questions/2286648/named-placeholders-in-string-formatting
public class StringFormat {
    public static String format(String format, Map<String, Object> values) {
        StringBuilder formatter = new StringBuilder(format);
        List<Object> valueList = new ArrayList<Object>();

        Matcher matcher = Pattern.compile("\\$\\{(\\w+)}").matcher(format);

        while (matcher.find()) {
            String key = matcher.group(1);

            String formatKey = String.format("${%s}", key);
            int index = formatter.indexOf(formatKey);

            if (index != -1) {
                formatter.replace(index, index + formatKey.length(), "%s");
                valueList.add(values.get(key));
            }
        }

        return String.format(formatter.toString(), valueList.toArray());
    }
}
