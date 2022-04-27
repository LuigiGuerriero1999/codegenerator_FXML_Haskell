package com.example.testboy.structures;


import com.example.testboy.orientation.Orientation;

public class Box extends GTKWidget{
    private int spacing;
    private Orientation orientation;

    public Box(String id, Integer id_hash, String name, double layoutX, double layoutY, int spacing, Orientation orientation) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
        this.spacing = spacing;
        this.orientation = orientation;
    }

    public int getSpacing() {
        return spacing;
    }

    public void setSpacing(int spacing) {
        this.spacing = spacing;
    }

    public Orientation getOrientation() {
        return orientation;
    }

    public void setOrientation(Orientation orientation) {
        this.orientation = orientation;
    }

    @Override
    public String gtkHsCode(){
        String boxConstructor = super.getName() + " <- Gtk.boxNew Gtk." + orientation + " " + spacing + "\n  ";
        String GtkHsCode = boxConstructor + "\n  ";
        return GtkHsCode;
    }
}