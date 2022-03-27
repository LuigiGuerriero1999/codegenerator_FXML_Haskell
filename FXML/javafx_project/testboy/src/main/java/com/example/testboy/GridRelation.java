package com.example.testboy;

import java.util.ArrayList;

public class GridRelation extends Relation{
    private int row;
    private int column;

    public GridRelation(int row, int column) {
        this.row = row;
        this.column = column;
    }

    public GridRelation(Integer parentID, Integer childID, int row, int column) {
        super(parentID, childID);
        this.row = row;
        this.column = column;
    }

    public String generateGtkHsCode(ArrayList<Object> allwidgets){
        return "";
    }
}
