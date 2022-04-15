package com.example.testboy;

import java.util.ArrayList;

public class GridRelation extends Relation{
    private int row;
    private int column;

    public GridRelation(int row, int column) {
        this.row = row;
        this.column = column;
    }

    public GridRelation(String parentName, String childName, int row, int column) {
        super(parentName, childName);
        this.row = row;
        this.column = column;
    }

    @Override
    public String generateGtkHsCode(){
        String haskellCode = "Gtk.gridAttach "+getParentName()+" "+getChildName()+" "+column+" "+row+" 1 1 \n  ";
        return haskellCode;
    }


}
