class PR15769 {
        private boolean foo () { return false; }


        public boolean bar (double blaz)
        {       
                return (Double.POSITIVE_INFINITY != blaz) && foo ();
        }
}

