package com.example.testboy.structures;

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
        String gridConstructor = super.getName() + " <- Gtk.gridNew\n  ";
        String columnHomogeneous = "Gtk.gridSetColumnHomogeneous " + super.getName() + " True\n  ";
        String rowHomogeneous = "Gtk.gridSetRowHomogeneous " + super.getName() + " True\n  ";

        String setColumnSpacing = "" ;
        if ((int)columnSpacing != 0) setColumnSpacing = "Gtk.gridSetColumnSpacing " + super.getName() + " " + (int)columnSpacing + "\n  ";

        String setRowSpacing = "" ;
        if ((int)rowSpacing != 0) setRowSpacing = "Gtk.gridSetRowSpacing " + super.getName() + " " + (int)rowSpacing + "\n  ";

        String GtkHsCode = gridConstructor + columnHomogeneous + rowHomogeneous + setColumnSpacing + setRowSpacing + "\n  ";
        return GtkHsCode;
    }
}
