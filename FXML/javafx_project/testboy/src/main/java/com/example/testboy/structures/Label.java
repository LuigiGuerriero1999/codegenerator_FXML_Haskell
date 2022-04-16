package com.example.testboy.structures;

public class Label extends GTKWidget{
    private String text;

    public Label(String text, double layoutX, double layoutY) {
        this.text = text;
        setLayoutX(layoutX);
        setLayoutY(layoutY);
    }

    public Label(String id, Integer id_hash, String name, String text, double layoutX, double layoutY) {
        super(id, id_hash, makeName(id,id_hash,name), layoutX, layoutY);
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String gtkHsCode(){
        String labelGtkHsCode = super.getName() + " <- Gtk.labelNew (Just " + "\"" + text + "\"" + ")\n  " + "\n  ";
        return labelGtkHsCode;
    }
}
