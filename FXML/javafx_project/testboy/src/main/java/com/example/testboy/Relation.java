package com.example.testboy;

import java.util.ArrayList;

public class Relation {
    private Integer parentID;
    private Integer childID;

    public Relation(){

    }

    public Relation(Integer parentID, Integer childID) {
        this.parentID = parentID;
        this.childID = childID;
    }

    public Integer getParentID() {
        return parentID;
    }

    public void setParentID(Integer parentID) {
        this.parentID = parentID;
    }

    public Integer getChildID() {
        return childID;
    }

    public void setChildID(Integer childID) {
        this.childID = childID;
    }

}
