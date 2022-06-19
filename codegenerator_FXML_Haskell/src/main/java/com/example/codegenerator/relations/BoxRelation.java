package com.example.codegenerator.relations;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;

public class BoxRelation extends Relation {
    private int interSpacing;

    public BoxRelation(String parentName, String childName, int interSpacing) {
        super(parentName, childName);
        this.interSpacing = interSpacing;
    }

    public int getInterSpacing() {
        return interSpacing;
    }

    public void setInterSpacing(int interSpacing) {
        this.interSpacing = interSpacing;
    }

    @Override
    public String generateGtkHsCode(){
        String template = "Gtk.boxPackStart ${PARENTNAME} ${CHILDNAME} True True ${INTERSPACING}\n  ";
        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("PARENTNAME", getParentName());
        toInsert.put("CHILDNAME", getChildName());
        toInsert.put("INTERSPACING", interSpacing);

        return StringFormat.format(template,toInsert);
    }
}