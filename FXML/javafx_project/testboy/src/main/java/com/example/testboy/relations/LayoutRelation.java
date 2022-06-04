package com.example.testboy.relations;

import com.example.testboy.StringFormat;

import java.util.HashMap;
import java.util.Map;

public class LayoutRelation extends Relation{
    private Integer x;
    private Integer y;
    public LayoutRelation() {
    }

    public LayoutRelation(String parentName, String childName, Integer x, Integer y) {
        super(parentName, childName);
        this.x = x;
        this.y = y;
    }

    public Integer getX() {
        return x;
    }

    public void setX(Integer x) {
        this.x = x;
    }

    public Integer getY() {
        return y;
    }

    public void setY(Integer y) {
        this.y = y;
    }

    @Override
    public String generateGtkHsCode(){
        String template = "Gtk.layoutPut ${PARENTNAME} ${CHILDNAME} ${X} ${Y}\n  ";
        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("PARENTNAME", getParentName());
        toInsert.put("CHILDNAME", getChildName());
        toInsert.put("X", x);
        toInsert.put("Y", y);

        return StringFormat.format(template,toInsert);
    }
}
