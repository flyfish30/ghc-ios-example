//
//  ContentView.swift
//  ghc-example
//
//  Created by Changsheng Liu on 2020/3/5.
//  Copyright © 2020 Changsheng Liu. All rights reserved.
//

import SwiftUI

struct ContentView: View {
    var body: some View {
        let hsStr = String(cString: UnsafePointer<CChar>(hello()))
        return Text(hsStr)
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
