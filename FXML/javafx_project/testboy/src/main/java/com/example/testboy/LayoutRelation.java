package com.example.testboy;

import com.example.testboy.structures.Button;
import com.example.testboy.structures.Entry;
import com.example.testboy.structures.GTKWidget;
import com.example.testboy.structures.Label;

import java.util.ArrayList;

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

    @Override
    public String generateGtkHsCode(){
        String haskellCode = "Gtk.layoutPut "+getParentName()+" "+getChildName()+" "+x+" "+y+"\n  ";
        return haskellCode;
    }
}
