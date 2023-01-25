import Image from 'next/image'
import { Inter } from '@next/font/google'
//import styles from './page.module.css'

/* Combination of html and javascript file .tsx 
   x at end means it also contains executable */

/* For Server side render pages are put on app directory */
/* For Client side render pages are put on page directory */

const inter = Inter({ subsets: ['latin'] })

const a = 5;

export default function Home() {
  console.log("On Server Side");
  return (
    <div>
    <div>Hello World from Zia Khan</div>
    <div>Second Line {a}</div>
    </div>
  )
}
