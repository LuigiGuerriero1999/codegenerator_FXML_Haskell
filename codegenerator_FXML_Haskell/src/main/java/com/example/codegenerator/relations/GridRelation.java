package com.example.codegenerator.relations;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;

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

    public int getRow() {
        return row;
    }

    public void setRow(int row) {
        this.row = row;
    }

    public int getColumn() {
        return column;
    }

    public void setColumn(int column) {
        this.column = column;
    }

    @Override
    public String generateGtkHsCode(){
        String template = "Gtk.gridAttach ${PARENTNAME} ${CHILDNAME} ${COLUMN} ${ROW} 1 1 \n  ";
        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("PARENTNAME", getParentName());
        toInsert.put("CHILDNAME", getChildName());
        toInsert.put("COLUMN", column);
        toInsert.put("ROW", row);

        return StringFormat.format(template,toInsert);
    }


}
