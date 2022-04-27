package com.example.testboy.relations;

import com.example.testboy.relations.Relation;

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
        String haskellCode = "Gtk.boxPackStart "+getParentName()+" "+getChildName()+" True True "+interSpacing+"\n  ";
        return haskellCode;
    }
}