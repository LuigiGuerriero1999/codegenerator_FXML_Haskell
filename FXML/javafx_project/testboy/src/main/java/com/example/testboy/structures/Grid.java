package com.example.testboy.structures;

public class Grid extends GTKWidget {
    public Grid(String id, Integer id_hash, String name, double layoutX, double layoutY) {
        super(id, id_hash, makeName(id,id_hash,name));
        setLayoutX(layoutX);
        setLayoutY(layoutY);
    }

    public Grid() {
    }

    public String gtkHsCode(){
        String gridConstructor = super.getName() + " <- Gtk.gridNew\n  ";
        String columnHomogeneous = "Gtk.gridSetColumnHomogeneous "+getName()+" True\n  ";
        String rowHomogeneous = "Gtk.gridSetRowHomogeneous "+getName()+" True\n  ";
        String GtkHsCode = gridConstructor + columnHomogeneous + rowHomogeneous + "\n  ";
        return GtkHsCode;
    }

}
