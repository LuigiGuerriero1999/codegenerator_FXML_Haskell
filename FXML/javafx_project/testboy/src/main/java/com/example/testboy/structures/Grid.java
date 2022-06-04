package com.example.testboy.structures;

import com.example.testboy.StringFormat;

import java.util.HashMap;
import java.util.Map;

public class Grid extends GTKWidget {
    private double columnSpacing;
    private double rowSpacing;

    public Grid(double columnSpacing, double rowSpacing) {
        this.columnSpacing = columnSpacing;
        this.rowSpacing = rowSpacing;
    }

    public Grid(String id, Integer id_hash, String name, double layoutX, double layoutY, double columnSpacing, double rowSpacing) {
        super(id, id_hash, makeName(id,id_hash,name), layoutX, layoutY);
        this.columnSpacing = columnSpacing;
        this.rowSpacing = rowSpacing;
    }

    public double getColumnSpacing() {
        return columnSpacing;
    }

    public void setColumnSpacing(double columnSpacing) {
        this.columnSpacing = columnSpacing;
    }

    public double getRowSpacing() {
        return rowSpacing;
    }

    public void setRowSpacing(double rowSpacing) {
        this.rowSpacing = rowSpacing;
    }

    @Override
    public String gtkHsCode(){
        StringBuilder template = new StringBuilder();
        template.append("${GRIDNAME} <- Gtk.gridNew\n  ");
        template.append("Gtk.gridSetColumnHomogeneous ${GRIDNAME} True\n  ");
        template.append("Gtk.gridSetRowHomogeneous ${GRIDNAME} True\n  ");
        if ((int)columnSpacing != 0) template.append("Gtk.gridSetColumnSpacing ${GRIDNAME} ${COLUMNSPACING}\n  ");
        if ((int)rowSpacing != 0) template.append("Gtk.gridSetRowSpacing ${GRIDNAME} ${ROWSPACING}\n  ");
        template.append("\n  ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("GRIDNAME", super.getName());
        toInsert.put("COLUMNSPACING", (int)columnSpacing);
        toInsert.put("ROWSPACING", (int)rowSpacing);

        return StringFormat.format(template.toString(), toInsert);
    }
}
