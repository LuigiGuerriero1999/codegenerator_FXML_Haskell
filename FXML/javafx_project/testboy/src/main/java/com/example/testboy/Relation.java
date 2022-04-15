package com.example.testboy;

import com.example.testboy.structures.GTKWidget;

import java.util.ArrayList;

public abstract class Relation {
    private String parentName;
    private String childName;

    public Relation(){

    }

    public Relation(String parentName, String childName) {
        this.parentName = parentName;
        this.childName = childName;
    }

    public String getParentName() {
        return parentName;
    }

    public void setParentName(String parentName) {
        this.parentName = parentName;
    }

    public String getChildName() {
        return childName;
    }

    public void setChildName(String childName) {
        this.childName = childName;
    }

    public String generateGtkHsCode(){
        return "";
    }

}
